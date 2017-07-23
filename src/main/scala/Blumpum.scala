import com.typesafe.config.ConfigFactory
import org.jsoup.Jsoup
import scalaj.http.Http
import scalaj.http.Base64

object Constants {
  val BaseApiUrl = "https://api.pinboard.in/v1"
  val DefaultNumberOfPosts = 10
  val UntitledDescriptions = Set("", "untitled")
}

object Blumpum extends App {

  def getBasicAuthHeader() = {
    val conf = ConfigFactory.load()
    val username = conf.getString("blumpum.username")
    val password = conf.getString("blumpum.password")
    val usernamePasswordEncoded = Base64.encodeString(s"$username:$password")
    ("Authorization", s"Basic $usernamePasswordEncoded")
  }

  def getDescriptions(posts: Seq[xml.Node]): Seq[String] = {
    posts.map(post => post.attribute("description").getOrElse("<no description>").toString)
  }

  def getPosts(numberOfPosts: Int) = {
    val basicAuthHeader = getBasicAuthHeader()

    val response = Http(s"${Constants.BaseApiUrl}/posts/all")
      .param("results", s"$numberOfPosts")
      .headers(basicAuthHeader)
      .asString

    if (response.code == 200) {
      val postsXML = xml.XML.loadString(response.body)
      postsXML.child.filter(c => c.attributes.nonEmpty)
    } else {
      throw new Error(s"Not OK response code: ${response.code}")
    }
  }

  def getPostTitle(postLink: String) = {
    val document = Jsoup.connect(postLink).get()
    document.select("title").text
  }

  def getUntitledPosts() = {
    val allPosts = getPosts(0)
    allPosts.filter(post => Constants.UntitledDescriptions.contains(
      getFirstAttributeAsString(post, "description")))
  }

  def updatePost(url: String, description: String) = {
    val basicAuthHeader = getBasicAuthHeader()

    Http(s"${Constants.BaseApiUrl}/posts/add")
      .param("url", url).param("description", description)
      .headers(basicAuthHeader)
      .asString
  }

  def getFirstAttributeAsString(node: xml.Node, attribute: String) = {
    node.attribute(attribute).get.head.toString
  }

  def getTitleAndUpdatePost(post: xml.Node) = {
    val postLink = getFirstAttributeAsString(post, "href")
    val postTitle = getPostTitle(postLink)

    updatePost(postLink, postTitle)
  }

  def setDescriptionForUntitledPosts() {
    val untitledPosts = getUntitledPosts()
    untitledPosts.foreach(getTitleAndUpdatePost)
  }

  val numberOfPosts = if (args.isEmpty) Constants.DefaultNumberOfPosts else args.head.toInt

  val posts = getPosts(numberOfPosts)
  val descriptions = getDescriptions(posts)

  println(posts.mkString("\n"))
}
