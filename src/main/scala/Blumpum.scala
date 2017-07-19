import com.typesafe.config.ConfigFactory
import scalaj.http.Http
import scalaj.http.Base64

object Constants {
  val BaseApiUrl = "https://api.pinboard.in/v1"
  val DefaultNumberOfPosts = 10
}

object Blumpum extends App {

  def getBasicAuthHeaderValue(username: String, password: String) = {
    val usernamePasswordEncoded = Base64.encodeString(s"$username:$password")
    s"Basic $usernamePasswordEncoded"
  }

  def getDescriptions(posts: Seq[xml.Node]): Seq[String] = {
    posts.map(post => post.attribute("description").getOrElse("<no description>").toString)
  }

  val conf = ConfigFactory.load()
  val username = conf.getString("blumpum.username")
  val password = conf.getString("blumpum.password")

  val numberOfPosts = if (args.isEmpty) Constants.DefaultNumberOfPosts.toString else args.head

  val basicAuthValue = getBasicAuthHeaderValue(username, password)

  val response = Http(s"${Constants.BaseApiUrl}/posts/all").param("results", numberOfPosts)
    .header("Authorization", basicAuthValue).asString

  if (response.code == 200) {
    val postsXML = xml.XML.loadString(response.body)
    val posts = postsXML.child.filter(c => c.attributes.nonEmpty)
    val descriptions = getDescriptions(posts)
    println(descriptions.mkString("\n"))
  } else {
    throw new Error(s"Not OK response code: ${response.code}")
  }
}
