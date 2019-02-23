package com.neo.sk.NiceBeat.common

import java.util.concurrent.TimeUnit

import com.neo.sk.NiceBeat.core.game.NiceBeatConfigServerImpl
import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.utils.SessionSupport.SessionConfig
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
/**
  * Created by hongruying on 2018/3/11
  *
  * edited by wmy
  */
object AppSettings {

  private implicit class RichConfig(config: Config){

    val noneValue = "none"

    def getOptionalString(path: String): Option[String] =
      if (config.getAnyRef(path) == noneValue) None
      else Some(config.getString(path))

    def getOptionalLong(path: String): Option[Long] =
      if (config.getAnyRef(path) == noneValue) None
      else Some(config.getLong(path))

    def getOptionalDurationSeconds(path: String): Option[Long] =
      if (config.getAnyRef(path) == noneValue) None
      else Some(config.getDuration(path, TimeUnit.SECONDS))
  }


  val log = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.parseResources("product.conf").withFallback(ConfigFactory.load())
  val niceBeatConfig:NiceBeatConfig = NiceBeatConfigServerImpl(ConfigFactory.parseResources("niceBeat.conf"))



  val appConfig = config.getConfig("app")
  val dependence = config.getConfig("dependence")

  val personLimit = appConfig.getInt("gameRoomManager.personLimit")



  val appSecureMap = {
    import collection.JavaConverters._
    val appIdList = collectionAsScalaIterable(appConfig.getStringList("client.appIds"))
    val secureKeys = collectionAsScalaIterable(appConfig.getStringList("client.secureKeys"))
    require(appIdList.size == secureKeys.size, "appIdList.length and secureKeys.length not equal.")
    appIdList.zip(secureKeys).toMap
  }




  val httpInterface = appConfig.getString("http.interface")
  val httpPort = appConfig.getInt("http.port")

  val serverProtocol = appConfig.getString("server.protocol")
  val serverHost = appConfig.getString("server.host")
  val rootPath = appConfig.getString("server.rootPath")
  val baseUrl = serverProtocol + "://" + serverHost


  val authCheck = appConfig.getBoolean("authCheck")
  val ramblerAuthCheck = appConfig.getBoolean("ramblerAuthCheck")

  val gameDataDirectoryPath = appConfig.getString("gameDataDirectoryPath")
  val gameRecordIsWork = appConfig.getBoolean("gameRecordIsWork")
  val gameRecordTime = appConfig.getInt("gameRecordTime")


//  val slickConfig = config.getConfig("slick.db")
//  val slickUrl = slickConfig.getString("url")
//  val slickUser = slickConfig.getString("user")
//  val slickPassword = slickConfig.getString("password")
//  val slickMaximumPoolSize = slickConfig.getInt("maximumPoolSize")
//  val slickConnectTimeout = slickConfig.getInt("connectTimeout")
//  val slickIdleTimeout = slickConfig.getInt("idleTimeout")
//  val slickMaxLifetime = slickConfig.getInt("maxLifetime")


//  private val ramblerConfig = appConfig.getConfig("rambler")
//  val ramblerAppId = ramblerConfig.getString("appId")
//  val ramblerSecureKey = ramblerConfig.getString("secureKey")
//  val ramblerProtocol = ramblerConfig.getString("protocol")
//  val ramblerHost = ramblerConfig.getString("host")
//  val ramblerPort = ramblerConfig.getInt("port")
//  val ramblerDomain = ramblerConfig.getString("domain")
//  val ramblerRootUrl = ramblerConfig.getString("rootUrl")
//
//  private val esheepConfig = appConfig.getConfig("esheep")
//  val esheepAppId = esheepConfig.getString("appId")
//  val esheepSecureKey = esheepConfig.getString("secureKey")
//  val esheepProtocol = esheepConfig.getString("protocol")
//  val esheepHost = esheepConfig.getString("host")
//  val esheepPort = esheepConfig.getInt("port")
//  val esheepDomain = esheepConfig.getString("domain")
//  val esheepGameId = esheepConfig.getLong("gameId")
//  val esheepGameKey = esheepConfig.getString("gsKey")
//  val esheepAuthToken = esheepConfig.getBoolean("authToken")





  val sessionConfig = {
    val sConf = config.getConfig("session")
    SessionConfig(
      cookieName = sConf.getString("cookie.name"),
      serverSecret = sConf.getString("serverSecret"),
      domain = sConf.getOptionalString("cookie.domain"),
      path = sConf.getOptionalString("cookie.path"),
      secure = sConf.getBoolean("cookie.secure"),
      httpOnly = sConf.getBoolean("cookie.httpOnly"),
      maxAge = sConf.getOptionalDurationSeconds("cookie.maxAge"),
      sessionEncryptData = sConf.getBoolean("encryptData")
    )
  }

  object MpAuthorConfig {
    private val conf = dependence.getConfig("mpAuthor")
    val protocol = conf.getString("protocol")
    val host = conf.getString("host")
    val port = conf.getString("port")
    val appId = conf.getString("appId")
    val secureKey = conf.getString("secureKey")
    val componentAppId = conf.getString("componentAppId")
    val mpAppId = conf.getString("mpAppId")
  }

  private val upperRainbowConfig = appConfig.getConfig("upperRainbowConf")
  val upperRainbowAppId = upperRainbowConfig.getString("appId")
  val upperRainbowSecureKey = upperRainbowConfig.getString("secureKey")
  //  val upperRainbowProtocol = upperRainbowConfig.getString("protocol")
  val upperRainbowHost = upperRainbowConfig.getString("host")
  val upperRainbowPort = upperRainbowConfig.getInt("port")

  object UpperRainbowConfig{
    val isWorkConfig = upperRainbowConfig.getBoolean("isWork")
    val appIdConfig = upperRainbowConfig.getString("appId")
    val secureKeyConfig = upperRainbowConfig.getString("secureKey")
    val hostConfig = upperRainbowConfig.getString("host")
    val portConfig = upperRainbowConfig.getString("port")
    object EventConfig{
      val eventFetcherConfig = upperRainbowConfig.getConfig("eventFetcher")
      val eachFetchNum = eventFetcherConfig.getInt("eachFetchNum")
      val idleIntervalTime = eventFetcherConfig.getLong("idleIntervalTime")
      val busyIntervalTime = eventFetcherConfig.getLong("busyIntervalTime")
    }

    object UserInfoConfig{
      val userInfoFetcherConfig = upperRainbowConfig.getConfig("userInfoFetcher")
      val eachFetchNum = userInfoFetcherConfig.getInt("eachFetchNum")
      val idleIntervalTime = userInfoFetcherConfig.getLong("idleIntervalTime")
      val busyIntervalTime = userInfoFetcherConfig.getLong("busyIntervalTime")
    }

  }

  val adminAccount = {
    import collection.JavaConverters._
    val list = appConfig.getStringList("adminAccount").asScala
    val admins = new ListBuffer[(Long,String,String)]
    for(i <- list.indices){
      val (account,pwd) = (list(i).split(":")(0),list(i).split(":")(1))
      admins.append((i+1,account,pwd))
    }
    admins.toList
  }

  val essfMapKeyName = "essfMap"






}
