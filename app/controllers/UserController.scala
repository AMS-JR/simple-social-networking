package controllers

import models.{Counter, Global, Like, Post, PostDao, User, UserDao}
import play.api.data.Forms._
import play.api.data._
import play.api.mvc._

import javax.inject.{Inject, Singleton}
import scala.collection.mutable.ListBuffer

@Singleton
class UserController @Inject()(val cc: MessagesControllerComponents,
                               userDao: UserDao,
                               postDao: PostDao,
                               authenticatedUserAction: AuthenticatedUserAction
                              ) extends MessagesAbstractController(cc) {

  private val logger = play.api.Logger(this.getClass)

  /**
   *
   */
  def login() = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.login(userDao.form))
  }
  /**
   * process the login attempt
   */
  def process() =  Action { implicit request: MessagesRequest[AnyContent] =>
    userDao.form.bindFromRequest().fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.login(formWithErrors))
      },
      user => {
        /* binding success, you get the actual value. */
        val foundUser = userDao.find(user)
        if (foundUser) {
          Redirect(routes.HomeController.index())
            .flashing("info" -> "You are logged in.")
            .withSession(Global.SESSION_USERNAME_KEY -> user.username)
        } else {
          Redirect(routes.UserController.login())
            .flashing("error" -> "Invalid username/password.")
        }
      }
    )
  }
  def logout() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    Redirect(routes.UserController.login())
      .flashing("info" -> "You are logged out.")
      .withNewSession
  }
  def signup() = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.signup(userDao.form))
  }
  def processSignup() = Action { implicit request: MessagesRequest[AnyContent] =>
    userDao.form.bindFromRequest().fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.signup(formWithErrors))
      },
      user => {
        /* binding success, you get the actual value. */
        val foundUser = userDao.find(user)
        if (foundUser) {
          Redirect(routes.UserController.signup())
            .flashing("error" -> "User already exist.")
        } else {
          //add user
          userDao.addUser(user)
          Redirect(routes.HomeController.index())
            .flashing("info" -> "You are logged in.")
            .withSession(Global.SESSION_USERNAME_KEY -> user.username)
        }
      }
    )
  }
//my profile
  def profile() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    /*Followers of authenticaticated User*/
    val current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    var posts = ListBuffer[Post]()
    var posts_ = ListBuffer[Map[String, Any]]()
    var comments = ListBuffer[Map[String, Any]]()
    var shares = ListBuffer[Map[String, Any]]()
    var likes = ListBuffer[Map[String, Any]]()
    var updatedLikes_ = ListBuffer[Map[String, Any]]()
    var updatedLikes = ListBuffer[Like]()
    var likesCount: Int = 0
    var following = ListBuffer[(String, Int)]() // counts for following, followers and posts
    var followers = ListBuffer[String]() // a list of the Users followers
    val action = request.flash.get("action").getOrElse("")
    val other_user = request.flash.get("otherUser").getOrElse("")
    //profile of user
    if(other_user.isEmpty){
      followers = userDao.findFollowers(Some(current_user))
      following += userDao.countFollowing(current_user)
      following += userDao.countFollowers(current_user)
      //get all posts for current user
      if(action == "remove"){
        val postId = request.flash.get("postId").getOrElse("")
        posts = postDao.removePost(current_user, postId)
        val postCount = posts.count(post => post.authorName == current_user)
        following += (("Posts", postCount))
      }else if(action == "like"){
        val postId = request.flash.get("postId").getOrElse("")
        //updated likes
        updatedLikes = postDao.likePost(current_user, postId.toInt)
        posts = postDao.findallPost(current_user)
        val postCount = posts.count(post => post.authorName == current_user)
        following += (("Posts", postCount))
      }else {
        posts = postDao.findallPost(current_user)
        val postCount = posts.count(post => post.authorName == current_user)
        following += (("Posts", postCount))
      }
    }else{
      if(action == "like"){
        val postId = request.flash.get("postId").getOrElse("")
        //updated likes
        updatedLikes = postDao.likePost(current_user, postId.toInt)
      }
      //get all posts for other user
      posts = postDao.findallPost(other_user)
      followers = userDao.findFollowers(Some(other_user))
      following += userDao.countFollowing(other_user)
      following += userDao.countFollowers(other_user)
      val postCount = posts.count(post => post.authorName == other_user)
      following += (("Posts", postCount))
    }
    //Sort the post by date
    posts = posts.sortBy(_.timestamp)(Ordering[java.util.Date].reverse)
    posts_ = posts.map(post => postDao.productToMap(post))
    posts_.foreach(post => {
      //get all comments
      comments = comments ++ postDao.findallComments(post("id"))
      //get all shares
      shares = shares ++ postDao.findallShares(post("id"))
      //get all Likes
      if(action == "like") {
        updatedLikes_ = updatedLikes_ ++ updatedLikes.filter(like => like.postId == post("id")).map(like => postDao.productToMap(like))
      }else {
        likes = likes ++ postDao.findallLikes(post("id"))
      }
    })
    //get all post action counts
    val actionCounts = posts_.map(post => {
      val postId = post("id").toString
      val shareCount = postDao.countShares_(shares, postId.toInt)
      val commentsCount = postDao.countComments(postId.toInt)
      if(action == "like") {
        likesCount = postDao.countLikes_(updatedLikes_, postId.toInt)
      }else{
        likesCount = postDao.countLikes_(likes, postId.toInt)
      }
      postDao.productToMap(Counter(postId.toInt,shareCount, likesCount, commentsCount))
    })
    val info = request.flash.get("info").getOrElse("")
//    if(info.isEmpty){
      Ok(views.html.profile(posts_,comments,actionCounts,following,followers,current_user, other_user))
//    }else { // trying to like a post again
//      Ok(views.html.profile(posts_,comments,actionCounts,following,followers,current_user, other_user))
//        .flashing("info" -> info)
//    }
  }
  //profiles of users you are following
  def userProfile(user: String) = authenticatedUserAction { implicit request =>
    Redirect(routes.UserController.profile())
      .flashing("otherUser" -> user)
  }
  //remove account
  def destroy() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    val current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    userDao.destroy(current_user)
    Redirect(routes.UserController.login())
      .flashing("info" -> "Your account is removed.")
      .withNewSession
  }
  def follow(user: String) = authenticatedUserAction { implicit request: Request[AnyContent] =>
    val current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    userDao.follow(current_user, user)
    Redirect(routes.UserController.userProfile(user))
  }
  def unfollow(user: String) = authenticatedUserAction { implicit request: Request[AnyContent] =>
    val current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    userDao.unfollow(current_user, user)
    Redirect(routes.UserController.userProfile(user))
  }
  //feeds
  def feed() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    /*Followers of authenticaticated User*/
    val username, current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    //get all post
    var posts = ListBuffer[Post]()
    var posts_ = ListBuffer[Map[String, Any]]()
    var comments = ListBuffer[Map[String, Any]]()
    var shares = ListBuffer[Map[String, Any]]()
    var likes = ListBuffer[Map[String, Any]]()
    var actionCounts = ListBuffer[Map[String, Any]]()
    var hashtags = ListBuffer[String]()
    var subscribedHashtags = List[String]()
    var topics = ListBuffer[String]()
    // topics/ followed hashtags
    topics = topics ++ userDao.findTopics(current_user)
    //get all posts
    if(request.flash.get("action").getOrElse("") == "search"){
      val param = request.flash.get("searchParam").head
      if(!param.isEmpty && param.contains("#")){ //search by hashtag
        posts = postDao.postsWithHashtag(param)
      }else{ //search by username
        posts = postDao.findallPost(param)
      }
    }else{
      topics.foreach(topic => posts = posts ++ postDao.postsWithHashtag(topic))
    }
    if(request.flash.get("sortBy").isEmpty){
      //Sort the post by date
      posts = posts.sortBy(_.timestamp)(Ordering[java.util.Date].reverse)
      posts_ = posts.map(post => postDao.productToMap(post))
    }else {
      //Sort the post by likes
      var count_ = List[(Int, Int)]()
      posts.foreach(post => {
        val count = postDao.countLikes(postDao.likes,post.id)
        count_ = count_ :+ (post.id, count)
      })
      val countOrderd = count_.sortBy(_._2)(Ordering[Int].reverse)
      var postsX = ListBuffer[Post]()
      //get the post sequentially by the likes count order in countOrderd
      countOrderd.foreach( count =>{
        postsX = postsX ++ posts.filter(post => post.id == count._1)
      })
      posts_ = postsX.map(post => postDao.productToMap(post))
    }
    posts_.foreach(post => {
      //get all comments
      comments = comments ++ postDao.findallComments(post("id"))
      //get all shares
      shares = shares ++ postDao.findallShares(post("id"))
      //get all Likes
      likes = likes ++ postDao.findallLikes(post("id"))
      //get all post action counts
      actionCounts += postDao.counts(post("id"))
      //get all the hashtags
      subscribedHashtags = subscribedHashtags :+ post("hashtag").toString
    })
    //get all the hashtags
    hashtags = hashtags ++  postDao.findHashtagsByRecency()
    Ok(views.html.feed(posts_,comments,actionCounts,hashtags,subscribedHashtags,topics,current_user))
  }
  def feedSortByLikes() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    Redirect(routes.UserController.feed())
      .flashing( "sortBy"-> "likes")
  }
  def subscribe(hashtag: String) = authenticatedUserAction { implicit request: Request[AnyContent] =>
    userDao.subscribe(request.session.get(Global.SESSION_USERNAME_KEY).getOrElse(""), hashtag)
    Redirect(routes.UserController.feed())
  }
  def unSubscribe(hashtag: String) = authenticatedUserAction { implicit request: Request[AnyContent] =>
    userDao.unSubscribe(request.session.get(Global.SESSION_USERNAME_KEY).getOrElse(""), hashtag)
    Redirect(routes.UserController.feed())
  }
}
