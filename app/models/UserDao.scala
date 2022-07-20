package models
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}

import javax.inject.Inject
import scala.collection.mutable.ListBuffer

case class User (username: String, password: String)
case class Follows(user: String, otherUser: String)

@javax.inject.Singleton
class UserDao @Inject()() {
  var users = Seq(
    User("user", "user"),
    User("user1", "user1"),
    User("user2", "user2"),
    User("user3", "user3"),
    User("user4", "user4"),
    User("admin", "admin")
  )
  var follows: ListBuffer[Follows] = ListBuffer(
    Follows("user1", "admin"),
    Follows("user2", "admin"),
    Follows("user3", "admin"),
    Follows("user1", "user2"),
    Follows("user1", "user3"),
    Follows("user2", "user1"),
    Follows("user2", "user3"),
    Follows("admin", "user3"),
    Follows("admin", "user2"),
    Follows("admin", "user1")
  )
  var topics: ListBuffer[(String, String)] = ListBuffer( // a tuple of a User and the hashtag the user is following
    ("admin", "#beautiful"),
    ("admin", "#kijamiigood"),
    ("user1", "#kijamiigood"),
    ("user2", "#tbt"),
    ("user3", "#photooftheday"),
    ("user3", "#tbt")
  )

  val form: Form[User] = Form (
    mapping(
      "username" -> nonEmptyText
        .verifying("too few chars",  s => lengthIsGreaterThanNCharacters(s, 2))
        .verifying("too many chars", s => lengthIsLessThanNCharacters(s, 20)),
      "password" -> nonEmptyText
        .verifying("too few chars",  s => lengthIsGreaterThanNCharacters(s, 2))
        .verifying("too many chars", s => lengthIsLessThanNCharacters(s, 30)),
    )(User.apply)(User.unapply)
  )
  private def lengthIsGreaterThanNCharacters(s: String, n: Int): Boolean = {
    if (s.length > n) true else false
  }

  private def lengthIsLessThanNCharacters(s: String, n: Int): Boolean = {
    if (s.length < n) true else false
  }
  /**
   * check if a user exists
   */
  def find(u: User): Boolean = {
    if (users.contains(u)) true else false
  }
  // map class values to key/value pairs
  def productToMap(cc: Product) = cc.productElementNames.zip(cc.productIterator).toMap

  def findFollowers(u: Option[String]): ListBuffer[String] = u match {
    case Some(u) => {
      var myMap1 =  ListBuffer[String]()
      follows.foreach(x =>
        if(u == x.user ) {myMap1 += x.otherUser;})
      myMap1
    }
    case None => ???
  }
  def findTopics(user: String) = {
    val topics_ = topics.filter(topic => topic._1 == user).map( topic => topic._2)
    topics_
  }
  def countFollowing(user: String): (String,Int) = {
    val following = follows.count(follow => follow.user == user)
    ("Following", following)
  }
  def countFollowers(user: String) = {
    val followers = follows.count(follow => follow.otherUser == user)
    ("Followers",followers)
  }
  def addUser(user: User) = {
    users = users :+ user
  }
  def destroy(current_user: String) = {
    users = users.filterNot(_.username == current_user)
  }
  def follow(current_user: String, user: String) = {
    follows += Follows(current_user, user)
  }
  def unfollow(current_user: String, user: String) = {
    follows -= Follows(current_user, user)
  }
  def subscribe(user: String, hashtag: String) = {
    topics += ((user, hashtag))
  }
  def unSubscribe(user: String, hashtag: String) = {
    topics -= ((user, hashtag))
  }
}
