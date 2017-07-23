import com.typesafe.config.ConfigFactory
import scalaj.http.Http
import scalaj.http.Base64

object Constants {
  val BaseApiUrl = "https://api.pinboard.in/v1"
  val DefaultNumberOfPosts = 10
}

object Blumpum extends App {

  def getBasicAuthHeaderValue() = {
    val conf = ConfigFactory.load()
    val username = conf.getString("blumpum.username")
    val password = conf.getString("blumpum.password")
    val usernamePasswordEncoded = Base64.encodeString(s"$username:$password")
    s"Basic $usernamePasswordEncoded"
  }

  def getDescriptions(posts: Seq[xml.Node]): Seq[String] = {
    posts.map(post => post.attribute("description").getOrElse("<no description>").toString)
  }

  def getPosts(numberOfPosts: Int) = {
    val basicAuthValue = getBasicAuthHeaderValue()

    val response = Http(s"${Constants.BaseApiUrl}/posts/all")
      .param("results", s"$numberOfPosts")
      .header("Authorization", basicAuthValue)
      .asString

    if (response.code == 200) {
      val postsXML = xml.XML.loadString(response.body)
      postsXML.child.filter(c => c.attributes.nonEmpty)
    } else {
      throw new Error(s"Not OK response code: ${response.code}")
    }
  }

  val numberOfPosts = if (args.isEmpty) Constants.DefaultNumberOfPosts else args.head.toInt

  val posts = getPosts(numberOfPosts)
  val descriptions = getDescriptions(posts)

  println(posts.mkString("\n"))
}
