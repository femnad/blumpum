import com.typesafe.config.ConfigFactory
import org.jsoup.Jsoup

import scalaj.http.{Base64, Http, HttpRequest, HttpResponse}

object Constants {
  val BaseApiUrl = "https://api.pinboard.in/v1"
  val DefaultNumberOfPosts = 10
  val UntitledDescriptions = Set("", "untitled")
}

case class Bookmark(url: String, title: String, tags: Array[String]) {
  override def toString: String = s"title: $title\nurl: $url\ntags: ${tags.mkString(",")}"
}

object Bookmark {
  import Blumpum.Post

  def getFirstAttributeAsString(node: Post, attribute: String): String = {
    node.attribute(attribute).get.head.toString
  }

  def getTitle(node: Post): String = getFirstAttributeAsString(node, "description")

  def getUrl(node: Post): String = getFirstAttributeAsString(node, "href")

  def getTags(node: Post): Array[String] = {
    getFirstAttributeAsString(node, "tag").split(" ").filterNot(tag => tag.isEmpty)
  }

  def getBookmarkFromPost(post: Post): Bookmark = {
    val title = Bookmark.getTitle(post)
    val url = Bookmark.getUrl(post)
    val tags = Bookmark.getTags(post)

    Bookmark(title, url, tags)
  }
}

object Blumpum extends App {

  type Post = xml.Node
  type Posts = Seq[Post]

  def buildBasicAuthHeader(): (String, String) = {
    val conf = ConfigFactory.load()
    val username = conf.getString("blumpum.username")
    val password = conf.getString("blumpum.password")
    val usernamePasswordEncoded = Base64.encodeString(s"$username:$password")
    ("Authorization", s"Basic $usernamePasswordEncoded")
  }

  def authenticatedGetRequest(url: String): HttpRequest = {
    Http(url).headers(buildBasicAuthHeader())
  }

  def getPosts(numberOfPosts: Int = 0): Posts = {
    val response = authenticatedGetRequest(s"${Constants.BaseApiUrl}/posts/all")
      .param("results", s"$numberOfPosts")
      .asString

    if (response.code == 200) {
      val postsXML = xml.XML.loadString(response.body)
      postsXML.child.filter(c => c.attributes.nonEmpty)
    } else {
      throw new Error(s"Not OK response code: ${response.code}")
    }
  }

  def getPostTitle(postLink: String): String = {
    val document = Jsoup.connect(postLink).get()
    document.select("title").text
  }

  def getUntitledPosts: Posts = {
    val allPosts = getPosts()
    allPosts.filter(post => Constants.UntitledDescriptions.contains(Bookmark.getTitle(post)))
  }

  def updatePost(url: String, description: String): HttpResponse[String] = {
    authenticatedGetRequest(s"${Constants.BaseApiUrl}/posts/add")
      .param("url", url).param("description", description)
      .asString
  }

  def getTitleAndUpdatePost(post: Post): HttpResponse[String] = {
    val postLink = Bookmark.getUrl(post)
    val postTitle = Bookmark.getTitle(post)

    updatePost(postLink, postTitle)
  }

  def setDescriptionForUntitledPosts(): Unit = {
    val untitledPosts = getUntitledPosts
    untitledPosts.foreach(getTitleAndUpdatePost)
  }

  def getUntaggedPosts: Posts = {
    getPosts().filter(post => Bookmark.getTags(post).isEmpty)
  }

  def suggestTagForPost(post: Post): HttpResponse[String] = {
    val url = Bookmark.getUrl(post)
    authenticatedGetRequest(s"${Constants.BaseApiUrl}/posts/suggest")
      .param("url", url).asString
  }

  val numberOfPosts = if (args.isEmpty) Constants.DefaultNumberOfPosts else args.head.toInt

  val posts = getPosts(numberOfPosts)
  val bookmarks = posts.map(Bookmark.getBookmarkFromPost)

  println(bookmarks.mkString("\n"))
}
