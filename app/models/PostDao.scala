package models
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, number, optional, text, tuple}

import java.util.{Calendar, Date}

import scala.collection.mutable.ListBuffer

case class Post(id: Int, authorName: String, timestamp: Date, content: String, picture: String, hashtag: String)
// authorName is the person who created the post
case class Comment(id: Int, postId: Int, authorName: String, timestamp: Date, content: String, picture: String)
// authorName is the person who commented on the post with id of postId
case class Like(author: String, postId: Int) // author is the person who liked the post with id of postId
case class Share(author: String, postId: Int) // author is the person who shared the post with id of postId
case class Counter(postId: Any, shareCount: Int, likesCount: Int, commentsCount: Int)

class PostDao {
  /**
   * contains all posts
   */
    var calender = Calendar.getInstance()
  var calender1 = Calendar.getInstance()
  var calender2 = Calendar.getInstance()
  var calender3 = Calendar.getInstance()
  var calender4 = Calendar.getInstance()
  calender.set(2022, 6, 1, 17, 50, 0)
  calender2.set(2022, 6, 2, 17, 50, 0)
  calender3.set(2022, 6, 3, 15, 50, 0)
  calender4.set(2022, 6, 4, 19, 50, 0)

  var posts = ListBuffer(
    Post(1,"admin", calender1.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut lacinia, lacus ut tristique vehicula, dolor mauris tempus ante, vel tristique elit urna ut tellus. Quisque aliquam risus eget leo malesuada, at dapibus lacus sagittis. Mauris in nibh ut arcu efficitur lobortis. Suspendisse potenti.",
      "aicha.jpg", "#photooftheday"),
    Post(2,"admin", calender1.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut lacinia, lacus ut tristique vehicula, dolor mauris tempus ante, vel tristique elit urna ut tellus. Quisque aliquam risus eget leo malesuada, at dapibus lacus sagittis. Mauris in nibh ut arcu efficitur lobortis. Suspendisse potenti.",
      "", "#like4like"),
    Post(3,"user1", calender2.getTime(), "",
      "ad.jpg", "#kijamiigood"),
    Post(4,"user1", calender2.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut lacinia, lacus ut tristique vehicula, dolor mauris tempus ante, vel tristique elit urna ut tellus. Quisque aliquam risus eget leo malesuada, at dapibus lacus sagittis. Mauris in nibh ut arcu efficitur lobortis. Suspendisse potenti.",
      "", "#tbt"),
    Post(5,"user2", calender3.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut lacinia, lacus ut tristique vehicula, dolor mauris tempus ante, vel tristique elit urna ut tellus. Quisque aliquam risus eget leo malesuada, at dapibus lacus sagittis. Mauris in nibh ut arcu efficitur lobortis. Suspendisse potenti.",
      "banner2.jpeg", "#beautiful"),
    Post(6,"user4", calender3.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut lacinia, lacus ut tristique vehicula, dolor mauris tempus ante, vel tristique elit urna ut tellus. Quisque aliquam risus eget leo malesuada, at dapibus lacus sagittis. Mauris in nibh ut arcu efficitur lobortis. Suspendisse potenti.",
      "", "#kijamiigood"),
    Post(7,"user4", calender4.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut lacinia, lacus ut tristique vehicula, dolor mauris tempus ante, vel tristique elit urna ut tellus. Quisque aliquam risus eget leo malesuada, at dapibus lacus sagittis. Mauris in nibh ut arcu efficitur lobortis. Suspendisse potenti.",
      "phone.jpg", "#beautiful")
  )
  /**
   * contains all post comments
   */
  var comments = ListBuffer(
    Comment(1,1,"user1", calender1.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", "ece.jpeg"),
    Comment(2,2,"user1", calender1.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", ""),
    Comment(3,1,"user2", calender1.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", ""),
    Comment(4,5,"user1", calender2.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", ""),
    Comment(5,2,"user2", calender2.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", ""),
    Comment(6,3,"user2", calender3.getTime(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", "")
  )
  /**
   * contains all post likes
   */
  var likes = ListBuffer(
    Like("user1", 1),
    Like("user1", 2),
    Like("user1", 5),
    Like("user2", 1),
    Like("user2", 2),
    Like("user2", 3),
    Like("admin", 3),
    Like("admin", 4)
  )

  /**
   * contains all post shares
   */
  var shares = ListBuffer(
    Share("user1", 1),
    Share("user1", 2),
    Share("user1", 5),
    Share("user2", 1),
    Share("user2", 2),
    Share("user2", 3),
    Share("admin", 3),
    Share("admin", 4)
  )

  val postFormTuple = Form (
    tuple(
      "content" -> text,
      "picture" -> text,
      "hashtag" -> nonEmptyText
    )
  )
  val commentFormTuple = Form (
    tuple(
      "postId" -> number,
      "page" -> text,
      "otherUser" -> text,
      "content" -> text,
      "picture" -> text
    )
  )
  /**
   * convert case class to Map
   * https://gist.github.com/lauris/7dc94fb29804449b1836
   */
  def productToMap(cc: Product) = cc.productElementNames.zip(cc.productIterator).toMap
  /**
   * get all post and relevant information
   */
    def counts(postId: Any) = {
      val shareCount = countShares(shares, postId)
      val likesCount = countLikes(likes,postId)
      val commentsCount = countComments(postId)
      productToMap(Counter(postId,shareCount, likesCount, commentsCount))
    }
  /**
   * TODO: combine these two methods by overloading. Same for countLikes and countLikes_
   *countShares(shareMap: Option[ListBuffer[Share]],shareMap_: Option[ListBuffer[Map[String, Any]] , postId: Any)
   */
  def countShares(shareMap: ListBuffer[Share], postId: Any): Int = {
    val cm = shareMap.map(productToMap)
    val sc = CountInstances(cm, postId)
    sc
  }
  def countShares_(shareMap: ListBuffer[Map[String, Any]], postId: Any): Int = {
    val sc = CountInstances(shareMap, postId)
    sc
  }
  def countLikes(likesMap: ListBuffer[Like],postId: Any): Int = {
    val cm = likesMap.map(productToMap)
    val lc = CountInstances(cm, postId)
    lc
  }
  def countLikes_(likesMap: ListBuffer[Map[String, Any]],postId: Any): Int = {
    val lc = CountInstances(likesMap, postId)
    lc
  }
  def countComments(postId: Any): Int = {
    val cm = comments.map(productToMap)
    val cc = CountInstances(cm, postId)
    cc
  }
  def CountInstances( productMap: ListBuffer[Map[String, Any]] ,postId: Any): Int = {
//    val cm = productMap.map(productToMap)
    var count = 0
    productMap.foreach(x =>
      if(x("postId") == postId ) {count += 1}
    )
    count
  }
  def findallPost(user: String) = { // all posts by user
    posts.filter(post => post.authorName == user)
  }
  def postsWithHashtag(topic: String) = {
    posts.filter(post => post.hashtag == topic)
  }
  def findHashtagsByRecency() = {
    posts.sortBy(_.timestamp)(Ordering[java.util.Date].reverse).map(post => post.hashtag).distinct.take(5)
  }
  def findallComments(postId: Any) = {
    comments.filter(comment => comment.postId == postId).map(comment => productToMap(comment))
  }
  def findallShares(postId: Any) = {
    shares.filter(share => share.postId == postId).map(share => productToMap(share))
  }
  def findallLikes(postId: Any) = {
    likes.filter(like => like.postId == postId).map(like => productToMap(like))
  }
  def postactionCount() = {
    posts.map(post => counts(post.id))
  }
  def likePost(user: String, postId: Int) = {
    likes += Like(user, postId)
    likes
  }
  def sharePost(user: String, postId: Int) = {
    shares += Share(user, postId)
    shares
  }
  def removePost(User: String, postId: String) = {
    posts = posts.filter(post => post.authorName == User).filterNot(post => post.id == postId.toInt)
    removeCommentOn(postId)
    removeLikesOn(postId)
    removeSharesOn(postId)
    posts
  }
  def removeCommentOn(postId: String) = {
    comments = comments.filterNot(comment => comment.postId == postId.toInt)
  }
  def removeLikesOn(postId: String) = {
    likes = likes.filterNot(like => like.postId == postId.toInt)
  }
  def removeSharesOn(postId: String) = {
    shares = shares.filterNot(share => share.postId == postId.toInt)
  }
  def addPost(authorName: String, content: String, hashtag: String, picture: String) = {
    posts += Post(8, authorName: String, calender.getTime(), content, picture, hashtag) // id = 8 here because am not updating to a database
  }
  def addComment( postId: Int, current_user: String, content: String, picture: String) = {
    comments += Comment(7, postId, current_user, calender.getTime(), content, picture) // id = 7 here because am not updating to a database
  }
}