import $ivy.`com.lihaoyi::scalatags:latest.integration`, scalatags.Text.all._
import $ivy.`com.atlassian.commonmark:commonmark:latest.integration`
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import org.commonmark.node.{Node, Paragraph, AbstractVisitor}
import java.time.Instant

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


def readPost(path: os.Path) = {
  val mdParser = Parser.builder.build
  Post(
    name = path.baseName,
    creationTime = getCreationTime(path),
    content = mdParser.parse(os.read(path))
  )
}

val formatTimeTagsScript: Frag = script("""
  for (let time of document.getElementsByTagName('time')) {
   time.textContent = new Date(time.dateTime).toLocaleString([], {year: 'numeric', month: 'numeric', day: 'numeric', hour: '2-digit', minute: '2-digit'});
  }
""")

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

def postSnippet(post: Post): Frag = {
  val mdRenderer = HtmlRenderer.builder.build
  tag("article")(
    h2(post.name),
    timeTag(post.creationTime),
    getFirstParagraph(post.content).map(n => raw(mdRenderer.render(n))),
    a(href := s"/${post.name}")("More")
  )
}
def generateHomePage(posts: IndexedSeq[Post]): String = {
  doctype("html")(html(
    head(
      tag("title")("Mah Test Blog"),
      defaultStyle
    ),
    body(
      h1("Mah Test Blog"),
      tag("main")(posts.map(postSnippet)),
      formatTimeTagsScript
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

@main
def main() = {
  val sourceRoot = os.pwd / "posts"
  val buildRoot = os.pwd / "docs"
  interp.watch(sourceRoot) // if run with amm --watch build.sc, watch the folder in addition to the script
  os.list(buildRoot).foreach(os.remove.all)
  
  val posts = os.list(sourceRoot).map(readPost).sortBy(_.creationTime).reverse
  os.write(buildRoot / "index.html", generateHomePage(posts))
  for (post <- posts) {
    val content = renderPost(post)
    os.write(buildRoot / s"${post.name}.html", content)
  }
}

