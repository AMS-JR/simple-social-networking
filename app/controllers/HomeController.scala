package controllers

import models.{Post, PostDao, UserDao}

import javax.inject._
import play.api._
import play.api.mvc._

import scala.collection.mutable.ListBuffer

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(
                                val controllerComponents: ControllerComponents,
                                userDao: UserDao,
                                postDao: PostDao,
                                authenticatedUserAction: AuthenticatedUserAction
                              ) extends BaseController {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  /**
   * This is the index page where the logged in authenticated user lands
   */
  def index() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    /*Followers of authenticaticated User*/
    val username = request.session.get(models.Global.SESSION_USERNAME_KEY)
    val current_user = username.head
    // users current user is following
    val following = userDao.findFollowers(username)
    var posts = ListBuffer[Post]()
    var posts_ = ListBuffer[Map[String, Any]]()
    var comments = ListBuffer[Map[String, Any]]()
    var shares = ListBuffer[Map[String, Any]]()
    var likes = ListBuffer[Map[String, Any]]()
    var actionCounts = ListBuffer[Map[String, Any]]()
    var hashtags = List[String]()
    //get all posts
    if(request.flash.get("action").getOrElse("") == "search"){
      val param = request.flash.get("searchParam").head
      if(!param.isEmpty && param.contains("#")){ //search by hashtag
        posts = postDao.postsWithHashtag(param)
      }else{ //search by username
        posts = postDao.findallPost(param)
      }
    }else{
      following.foreach( followedUser => posts = posts ++ postDao.findallPost(followedUser))
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
      //get all the hashtags -> only hashtags used by those you are following
      hashtags = hashtags :+ post("hashtag").toString
    })

      Ok(views.html.index(posts_,comments,actionCounts,following,hashtags.distinct.take(5),current_user))
  }
  def landingPage() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.landingPage("Kijamii"))
  }
  def sortByLikes() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    Redirect(routes.HomeController.index())
      .flashing( "sortBy"-> "likes")
  }
  def exploreSortByLikes() = authenticatedUserAction { implicit request: Request[AnyContent] =>
    Redirect(routes.HomeController.explore())
      .flashing( "sortBy"-> "likes")
  }

  def explore() = Action { implicit request: Request[AnyContent] =>
    /*Followers of authenticaticated User*/
    val username, current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    // users current user is following
    val following = userDao.findFollowers(Some(username))
    //get all post
    var posts = ListBuffer[Post]()
    var posts_ = ListBuffer[Map[String, Any]]()
    var comments = ListBuffer[Map[String, Any]]()
    var shares = ListBuffer[Map[String, Any]]()
    var likes = ListBuffer[Map[String, Any]]()
    var actionCounts = ListBuffer[Map[String, Any]]()
    var hashtags = ListBuffer[String]()
    //get all posts
    if(request.flash.get("action").getOrElse("") == "search"){
      val param = request.flash.get("searchParam").head
      if(!param.isEmpty && param.contains("#")){ //search by hashtag
        posts = postDao.postsWithHashtag(param)
      }else{ //search by username
        posts = postDao.findallPost(param)
      }
    }else{
      posts = postDao.posts
    }
    //get all the hashtags -> Here i consider all hashtags and take the most recent 5
    hashtags = hashtags ++  postDao.findHashtagsByRecency()
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
    }
    )
    Ok(views.html.explore(posts_,comments,actionCounts,following,hashtags,current_user))
  }
  def search() = Action { implicit request: Request[AnyContent] =>
    val current_user = request.session.get(models.Global.SESSION_USERNAME_KEY).getOrElse("")
    val postVals = request.body.asFormUrlEncoded
    postVals.map{ arg =>
      val page = arg("page").head
      val searchParam = arg("search").head
      if(page == "home"){
        Redirect(routes.HomeController.index())
          .flashing("action" -> "search", "searchParam" -> searchParam)
      }else if(page == "explore"){
        Redirect(routes.HomeController.explore())
          .flashing("action" -> "search", "searchParam" -> searchParam)
      }else{ //feed
        Redirect(routes.UserController.feed())
          .flashing("action" -> "search", "searchParam" -> searchParam)
      }
    }.getOrElse(Redirect(routes.HomeController.index()))
  }

}
