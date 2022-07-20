package controllers

import models.{Counter, Global, Like, Post, PostDao, Share, UserDao}
import play.api.mvc._

import java.nio.file.Paths
import javax.inject.Inject
import scala.collection.mutable.ListBuffer

class PostController @Inject()(
                                val cc: MessagesControllerComponents,
                                userDao: UserDao,
                                postDao: PostDao,
                                authenticatedUserAction: AuthenticatedUserAction
                              ) extends MessagesAbstractController(cc) {
  /**
   * Call updateLike to check that active user is not performing like()
   */
  def like(id: String, user: String, otherUser: String, page: String) = authenticatedUserAction { implicit request =>
    Redirect(routes.PostController.updateLike())
      .flashing("otherUser" -> otherUser, "page" -> page, "postId" -> id)
  }
  def updateLike() = Action { implicit request =>
    val post = request.flash.get("postId").getOrElse("")
    val c_user = request.session.get(Global.SESSION_USERNAME_KEY).getOrElse("")
      (c_user, post) match {
        case (u, p) =>
          if(postDao.likes.contains(Like(u, p.toInt))) {
            if (request.flash.get("page").getOrElse("") == "home") { //landingPage
              Redirect(routes.HomeController.index())
                .flashing("info" -> "You already liked this post")
            } else if (request.flash.get("page").getOrElse("") == "explore") { //explore
              Redirect(routes.HomeController.explore())
                .flashing("info" -> "You already liked this post")
            }else if(request.flash.get("page").getOrElse("") == "feed"){ //feeds
              Redirect(routes.UserController.feed())
                .flashing("info" -> "You already liked this post")
          }else{ //profile page
              val otherUser = request.flash.get("otherUser").getOrElse("")
              if(otherUser.isEmpty){
                Redirect(routes.UserController.profile())
                  .flashing("otherUser" -> request.flash.get("otherUser").getOrElse(""), "info" -> "You already liked this post")
              }else{
                Redirect(routes.UserController.profile())
                  .flashing("otherUser" -> request.flash.get("otherUser").getOrElse(""), "info" -> "You already liked this post")
              }
            }
        } else {
          if(request.flash.get("page").getOrElse("") == "profile"){ // has its own implementation of Like action
            val otherUser = request.flash.get("otherUser").getOrElse("")
            if(otherUser.isEmpty){
              Redirect(routes.UserController.profile())
                .flashing("action" -> "like", "postId" -> request.flash.get("postId").getOrElse(""))
            }else{
              Redirect(routes.UserController.profile())
                .flashing("otherUser" -> request.flash.get("otherUser").getOrElse(""), "action" -> "like",
                  "postId" -> request.flash.get("postId").getOrElse(""))
            }
          }else{
          //users currentUser is following
          val following = userDao.findFollowers(request.session.get(Global.SESSION_USERNAME_KEY))
          var posts = ListBuffer[Post]()
          var posts_ = ListBuffer[Map[String, Any]]()
          var comments = ListBuffer[Map[String, Any]]()
          var updatedLikes_ = ListBuffer[Map[String, Any]]()
          var shares = ListBuffer[Map[String, Any]]()
          var hashtags = List[String]() // for home page
          var hashtags_ = ListBuffer[String]() // for explore page
          var subscribedHashtags = List[String]()
          var topics = ListBuffer[String]()
          //get all posts
          if(request.flash.get("page").getOrElse("") == "explore"){
            posts = postDao.posts
          }else if(request.flash.get("page").getOrElse("") == "feed"){
            // topics/ followed hashtags
            topics = topics ++ userDao.findTopics(c_user)
            //get all posts
            topics.foreach(topic => posts = posts ++ postDao.postsWithHashtag(topic))
          }else{
            following.foreach(followedUser => posts = posts ++ postDao.findallPost(followedUser))
          }
          //Sort the post by date
          posts = posts.sortBy(_.timestamp)(Ordering[java.util.Date].reverse)
          posts_ = posts.map(post => postDao.productToMap(post))
          //updated likes
          val updatedLikes = postDao.likePost(u, p.toInt)
          posts_.foreach(post => {
            //get all comments
            comments = comments ++ postDao.findallComments(post("id"))
            updatedLikes_ = updatedLikes_ ++ updatedLikes.filter(like => like.postId == post("id")).map(like => postDao.productToMap(like))
            //get all shares
            shares = shares ++ postDao.findallShares(post("id"))
            if(request.flash.get("page").getOrElse("") == "home"){
              //get all the hashtags -> only hashtags used by those you are following
              hashtags = hashtags :+ post("hashtag").toString
            }
            if(request.flash.get("page").getOrElse("") == "feed"){
              //get all the hashtags
              subscribedHashtags = subscribedHashtags :+ post("hashtag").toString
            }
          })
          //get all post action counts
          val actionCounts = posts_.map(post => {
            val postId = post("id").toString
            val shareCount = postDao.countShares_(shares, postId.toInt)
            val likesCount = postDao.countLikes_( updatedLikes_ , postId.toInt)
            val commentsCount = postDao.countComments(postId.toInt)
            postDao.productToMap(Counter(postId.toInt,shareCount, likesCount, commentsCount))
          })
            if(request.flash.get("page").getOrElse("") == "explore"){
              //get all the hashtags
              hashtags_ = hashtags_ ++  postDao.findHashtagsByRecency()
              Ok(views.html.explore(posts_,comments,actionCounts,following, hashtags_, c_user))
            }else if(request.flash.get("page").getOrElse("") == "feed"){
              //get all the hashtags
              hashtags_ = hashtags_ ++  postDao.findHashtagsByRecency()
              Ok(views.html.feed(posts_,comments,actionCounts,hashtags_,subscribedHashtags,topics,c_user))
            }else{
              Ok(views.html.index(posts_,comments,actionCounts,following, hashtags, c_user))
            }
            }
        }
        case _ => ???
    }
  }
  /**
   * Share a post
   */
  def share(id: String, user: String, page: String)= authenticatedUserAction { implicit request =>
    Redirect(routes.PostController.updateShare())
      .flashing("page" -> page, "postId" -> id)
  }
  def updateShare() = Action { implicit request =>
    val post = request.flash.get("postId")
    val c_user = request.session.get(Global.SESSION_USERNAME_KEY).head
    (c_user, post) match {
      case (u, Some(p)) =>
        if(postDao.shares.contains(Share(u, p.toInt))) {
          if(request.flash.get("page").getOrElse("") == "home"){ //landingPage
            Redirect(routes.HomeController.index())
              .flashing("info" -> "You already shared this post")
          }else { //explore  NB: Share is disabled in profile page
            Redirect(routes.HomeController.explore())
              .flashing("info" -> "You already shared this post")
          }
        } else {
          val following = userDao.findFollowers(request.session.get(Global.SESSION_USERNAME_KEY))
          var posts = ListBuffer[Post]()
          var posts_ = ListBuffer[Map[String, Any]]()
          var comments = ListBuffer[Map[String, Any]]()
          var likes = ListBuffer[Map[String, Any]]()
          var updatedShares_ = ListBuffer[Map[String, Any]]()
          var hashtags = List[String]()
          var hashtags_ = ListBuffer[String]()
          var subscribedHashtags = List[String]()
          var topics = ListBuffer[String]()
          //get all posts
          if(request.flash.get("page").getOrElse("") == "explore"){
            posts = postDao.posts
          }else if(request.flash.get("page").getOrElse("") == "feed"){
            // topics/ followed hashtags
            topics = topics ++ userDao.findTopics(c_user)
            //get all posts
            topics.foreach(topic => posts = posts ++ postDao.postsWithHashtag(topic))
          }else{
            following.foreach(followedUser => posts = posts ++ postDao.findallPost(followedUser))
          }
          //Sort the post by date
          posts = posts.sortBy(_.timestamp)(Ordering[java.util.Date].reverse)
          posts_ = posts.map(post => postDao.productToMap(post))
//        // share post action. This action has to be here to avoid repeatition
          val updatedShares = postDao.sharePost(u, p.toInt)
          posts_.foreach(post => {
            //get all comments
            comments = comments ++ postDao.findallComments(post("id"))
            //get all Likes
            likes = likes ++ postDao.findallLikes(post("id"))
            //updated shares
            updatedShares_ = updatedShares_ ++ updatedShares.filter(share => share.postId == post("id")).map(share => postDao.productToMap(share))
            if(request.flash.get("page").getOrElse("") == "home"){
              //get all the hashtags -> only hashtags used by those you are following
              hashtags = hashtags :+ post("hashtag").toString
            }
            if(request.flash.get("page").getOrElse("") == "feed"){
              //get all the hashtags
              subscribedHashtags = subscribedHashtags :+ post("hashtag").toString
            }
          })
          //get all post action counts
          val actionCounts = posts_.map(post => {
            val postId = post("id").toString
            val shareCount = postDao.countShares_(updatedShares_, postId.toInt)
            val likesCount = postDao.countLikes_(likes, postId.toInt)
            val commentsCount = postDao.countComments(postId.toInt)
            postDao.productToMap(Counter(postId.toInt, shareCount, likesCount, commentsCount))
          })
          if(request.flash.get("page").getOrElse("") == "explore"){
            //get all the hashtags
            hashtags_ = hashtags_ ++  postDao.findHashtagsByRecency()
            Ok(views.html.explore(posts_,comments,actionCounts,following, hashtags_, c_user))
          }else if(request.flash.get("page").getOrElse("") == "feed"){
            //get all the hashtags
            hashtags_ = hashtags_ ++  postDao.findHashtagsByRecency()
            Ok(views.html.feed(posts_,comments,actionCounts,hashtags_,subscribedHashtags,topics,c_user))
          }else{
            Ok(views.html.index(posts_,comments,actionCounts,following, hashtags, c_user))
          }
        }
        case _ => ???
      }
  }
  /**
   * remove a post
   */
  def removePost(id: String) = authenticatedUserAction { implicit request =>
    Redirect(routes.UserController.profile())
      .flashing("action" -> "remove", "postId" -> id)
  }
  def create() = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.createPost(postDao.postFormTuple))
  }
  def newPost() = Action(parse.multipartFormData) { request =>
    val current_user = request.session.get(Global.SESSION_USERNAME_KEY).getOrElse("")
    val postVals = request.body.asFormUrlEncoded
    val content = postVals("content").head
    var hashtag = postVals("hashtag").head
    if(!hashtag.isEmpty){
      if(!hashtag.startsWith("#")){
        hashtag = "#".concat(hashtag)
      }
    }
    request.body
      .file("picture")
      .map { picture =>
        val filename    = Paths.get(picture.filename).getFileName
        val fileSize    = picture.fileSize
        val contentType = picture.contentType
        picture.ref.copyTo(Paths.get(s"public/tmp/picture/$filename"), replace = true)
        postDao.addPost(current_user, content, hashtag, filename.toString)
        Redirect(routes.UserController.profile())
          .flashing("info" -> "new post successfully added")
      }.getOrElse {
        postDao.addPost(current_user, content, hashtag, "")
        Redirect(routes.UserController.profile())
          .flashing("info" -> "new post successfully added")
      }
  }

      /**
       * since the comment functionality exist in the home,explore and profile pages, I passed not only the postId,
       * but also a page param to reference the the page to return to after adding the comment and also a otherUser parameter
       * to differentiate the two different versions of the profile page. Weather you are making the comment from your profile
       * or from the profile of other users you are viewing
       */
      def comment(postId: String, otherUser: String, page: String) = Action { implicit request: MessagesRequest[AnyContent] =>
        Ok(views.html.comment(postDao.commentFormTuple)(postId,otherUser,page))
      }
      def processComment() = Action(parse.multipartFormData) { request =>
        val current_user = request.session.get(Global.SESSION_USERNAME_KEY).getOrElse("")
        val postVals = request.body.asFormUrlEncoded
        val postId = postVals("postId").head
        val otherUser = postVals("otherUser").head
        val page = postVals("page").head
        val content = postVals("content").head

        request.body
          .file("picture")
          .map { picture =>
            val filename    = Paths.get(picture.filename).getFileName
            val fileSize    = picture.fileSize
            val contentType = picture.contentType
            picture.ref.copyTo(Paths.get(s"public/tmp/picture/$filename"), replace = true)
            postDao.addComment( postId.toInt,current_user, content, filename.toString)
            if(page == "home"){
              Redirect(routes.HomeController.index())
            }else if(page == "explore"){
              Redirect(routes.HomeController.explore())
            }else if(page == "feed"){
              Redirect(routes.UserController.feed())
            }else { // page is profile
              if (otherUser.isEmpty){ //profile current user
                Redirect(routes.UserController.profile())
              }else{ //profile of other users
                Redirect(routes.UserController.userProfile(otherUser))
              }
            }
          }
          .getOrElse {
            postDao.addComment( postId.toInt,current_user, content, "")
            if(page == "home"){
              Redirect(routes.HomeController.index())
            }else if(page == "explore"){
              Redirect(routes.HomeController.explore())
            }else { // page is profile
              if (otherUser.isEmpty){ //profile current user
                Redirect(routes.UserController.profile())
              }else{ //profile of other users
                Redirect(routes.UserController.userProfile(otherUser))
              }
            }
          }
      }
  def deleteComment() = TODO
}
