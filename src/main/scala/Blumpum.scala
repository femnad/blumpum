package blumpum

import com.typesafe.config.ConfigFactory
import org.jsoup.Jsoup

import scalaj.http.{Base64, Http, HttpRequest, HttpResponse}


object Constants {
  val BaseApiUrl = "https://api.pinboard.in/v1"
  val DefaultNumberOfBookmarks = 10
  val UntitledDescriptions = Set("", "untitled")
}

case class Bookmark(url: String, title: String, tags: Array[String]) {
  override def toString: String = s"title: $title\nurl: $url\ntags: ${tags.mkString(",")}"
}

object Bookmark {
  type Post = xml.Node

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

    Bookmark(url, title, tags)
  }
}

object Blumpum extends App {
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

  def getBookmarks(numberOfResults: Int = 0): Seq[Bookmark] = {
    val response = authenticatedGetRequest(s"${Constants.BaseApiUrl}/posts/all")
      .param("results", s"$numberOfResults")
      .asString

    if (response.code == 200) {
      val postsXML = xml.XML.loadString(response.body)
      postsXML.child.filter(c => c.attributes.nonEmpty)
        .map(Bookmark.getBookmarkFromPost)
    } else {
      throw new Error(s"Not OK response code: ${response.code}")
    }
  }

  def getBookmarkTitle(bookmark: Bookmark): String = {
    val bookmarkUrl = bookmark.url
    val document = Jsoup.connect(bookmarkUrl).get()
    document.select("title").text
  }

  def filterUntitledBookmarks(bookmarks: Seq[Bookmark]): Seq[Bookmark] = {
    bookmarks.filter(bookmark => Constants.UntitledDescriptions.contains(bookmark.title))
  }

  def updateBookmarkWithTitle(bookmark: Bookmark, title: String): HttpResponse[String] = {
    val url = bookmark.url
    // Store current tags to keep them intact after the update request
    val currentTags = bookmark.tags
    authenticatedGetRequest(s"${Constants.BaseApiUrl}/posts/add")
      .param("url", url).param("description", title)
      .param("tags", currentTags.mkString(","))
      .asString
  }

  def getTitleAndUpdateBookmark(bookmark: Bookmark): HttpResponse[String] = {
    val title = getBookmarkTitle(bookmark)
    updateBookmarkWithTitle(bookmark, title)
  }

  def setTitlesForUntitledBookmarks(untitledBookmarks: Seq[Bookmark]): Unit = {
    untitledBookmarks.foreach(getTitleAndUpdateBookmark)
  }

  def filterUntaggedBookmarks(bookmarks: Seq[Bookmark]): Seq[Bookmark] = {
    bookmarks.filter(bookmark => bookmark.tags.isEmpty)
  }

  def suggestTagForPost(bookmark: Bookmark): HttpResponse[String] = {
    authenticatedGetRequest(s"${Constants.BaseApiUrl}/posts/suggest")
      .param("url", bookmark.url).asString
  }

  def filterBookmarksByTag(bookmarks: Seq[Bookmark], tag: String): Seq[Bookmark] = {
    bookmarks.filter(bookmark => bookmark.tags.contains(tag))
  }

  val numberOfBookmarks = if (args.isEmpty) Constants.DefaultNumberOfBookmarks else args.head.toInt

  val bookmarks = getBookmarks(numberOfBookmarks)

  println(bookmarks.mkString("\n"))
}
