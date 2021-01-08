import $ivy.`com.lihaoyi::scalatags:latest.integration`, scalatags.Text.all._
import $ivy.`com.atlassian.commonmark:commonmark:latest.integration`
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import org.commonmark.node.{Node, Paragraph, AbstractVisitor}
import java.time.Instant
import mill._
import upickle.default._

val postNameToPath = interp.watchValue(os.list(millSourcePath / "posts").map(path => path.baseName -> path).toMap)
val post = new Cross[PostModule](postNameToPath.keys.toSeq: _*)
val posts = T.sequence(post.itemMap.values.map(_.render).toSeq)

case class RenderedPost(name: String, creationTime: Instant, contents: String, preview: String)
object RenderedPost {
  implicit val rw: upickle.default.ReadWriter[RenderedPost] = macroRW
}

implicit val instantReadWriter: ReadWriter[Instant] = readwriter[Long].bimap[Instant](
  instant => instant.toEpochMilli,
  ms => Instant.ofEpochMilli(ms)
)

class PostModule(name: String) extends Module {
  val srcPath = postNameToPath(name)
  //val destPath = T.dest / srcPath.last
  private def source = T.source(srcPath)
  def render = T{
    RenderedPost(name, Instant.now(), renderPost(readPost(source().path)), "")
  }
}

def site = T{
  for (postInfo <- posts()) {
    os.write(T.dest / s"${postInfo.name}.html", postInfo.contents)
  }
  os.write(T.dest / "index.html", generateHomePage(posts()))
  PathRef(T.dest)
}

def docs = T{
  val source = site().path
  val docs = os.pwd/"docs"
  os.list(docs).foreach(os.remove.all)
  os.list(source).foreach(os.copy.into(_, docs))
  PathRef(docs)
}

def getFirstParagraph(root: Node): Option[Paragraph] = {
  var result: Option[Paragraph] = None;
  root.accept(new AbstractVisitor {
    override def visit(p: Paragraph): Unit = {
      if (result.isDefined) return
      result = Some(p)
    }
  })
  result
}

def getGitAddTime(path: os.Path): Option[Instant] = {
  val gitLogCommand = s"git log --diff-filter=A --follow --format=%at -1 -- $path".split(" ").map[os.Shellable](identity)
  os.proc("git", "log", "--reverse", "--follow", "--format=%at", "--", path).call(check=false).out.lines.headOption.flatMap(_.toLongOption).map(Instant.ofEpochSecond)
}

def getCreationTime(path: os.Path): Instant = getGitAddTime(path).getOrElse(os.stat(path).mtime.toInstant())

case class Post(name: String, creationTime: Instant, content: Node)


def readPost(path: os.Path): Post = {
  val mdParser = Parser.builder.build
  Post(
    name = path.baseName,
    creationTime = getCreationTime(path),
    content = mdParser.parse(os.read(path))
  )
}

val formatTimeTagsScript: Frag = script(raw("""
  window.addEventListener('DOMContentLoaded', () => {
    for (let time of document.getElementsByTagName('time')) {
     time.textContent = new Date(time.dateTime).toLocaleString([], {year: 'numeric', month: 'numeric', day: 'numeric', hour: '2-digit', minute: '2-digit'});
    }
  });
"""))

def timeTag(time: Instant) = tag("time")(attr("datetime") := time.toString)

// Taken from http://bettermotherfuckingwebsite.com/
val defaultStyle: Frag = tag("style")(attr("type") := "text/css")("""
  body {
    margin:40px auto;
    max-width:650px;
    line-height:1.6;
    font-size:18px;
    color:#444;
    padding:0 10px
  }
  h1, h2, h3 {
    line-height:1.2;
  }
""")

def postSnippet(post: RenderedPost): Frag = {
  val mdRenderer = HtmlRenderer.builder.build
  tag("article")(
    h2(post.name),
    timeTag(post.creationTime),
    raw(post.preview),
    a(href := s"/${post.name}")("More")
  )
}
def generateHomePage(posts: Seq[RenderedPost]): String = {
  doctype("html")(html(
    head(
      tag("title")("Mah Test Blog"),
      defaultStyle,
      formatTimeTagsScript
    ),
    body(
      h1("Mah Test Blog"),
      tag("main")(posts.map(postSnippet))
    )
  )).render
}

def renderPost(post: Post): String = {
  doctype("html")(html(
    head(
      tag("title")(post.name),
      defaultStyle
    ),
    body(
      h1(post.name),
      timeTag(post.creationTime),
      raw(HtmlRenderer.builder.build.render(post.content)),
      formatTimeTagsScript
    )
  )).render
}

