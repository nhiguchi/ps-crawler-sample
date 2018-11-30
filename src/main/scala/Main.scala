import java.net.URL
import java.time.LocalDate

import javax.imageio.ImageIO
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.collection.JavaConverters._


object Main extends App {
  val SiteDomainUrl = "http://papimo.jp"
  val HallTopUrl = SiteDomainUrl + "/h/00031701/hit/top"

  println("Start.")
  val today = LocalDate.now()

  // ホールトップ
  val hallTopDoc = requestByGet(HallTopUrl)

  // ホールトップ → 機種一覧
  val modelListUrl = SiteDomainUrl + hallTopDoc
    .select(".menu-top li")
    .get(2)
    .select("a")
    .attr("href")
  println(s"modelListUrl=[$modelListUrl]")
  val modelListDoc = requestByPost(modelListUrl)

  // 機種一覧 → 台一覧
  val machineListUrls = modelListDoc
    .select("ul.item")
    .select("a")
    .asScala
    .map(SiteDomainUrl + _.attr("href"))
  println(s"machineListUrlsSize=[${machineListUrls.size}]")
  println(s"machineListUrls=[$machineListUrls]")
  val machineListDocs = machineListUrls.map(requestByGet)

  // 台一覧 → 台詳細
  val machineDetailUrls = machineListDocs.flatMap {
    _
      .select(".unit_no")
      .asScala
      .map(SiteDomainUrl + _.select("a").attr("href"))
  }
  println(s"machineDetailUrlsSize=[${machineDetailUrls.size}]")
  println(s"machineDetailUrls=[$machineDetailUrls]")
  val machineDetailDocs = machineDetailUrls.map(requestByGet)

  // 台詳細を取得
  val machineHistories = machineDetailDocs.map { doc =>
    val graphRelativeUrl = doc
      .select(".graph-some td")
      .last()
      .select("img")
      .attr("src")
    val graphUrl = graphRelativeUrl match {
      // 絶対パスで返ってくる場合と相対パスで返ってくる場合がある
      case url if url.startsWith("http") => url
      case url => SiteDomainUrl + url
    }
    MachineHistory(
      machineNo = doc.select(".unit_no").get(0).text().diff("番台").toInt,
      machineName = doc.select(".name").text(),
      bbCount = getFromMachineDetail(doc, "BB回数"),
      rbCount = getFromMachineDetail(doc, "RB回数"),
      totalStartCount = getFromMachineDetail(doc, "総スタート"),
      finalStartCount = getFromMachineDetail(doc, "最終スタート"),
      maxOutputMedal = getFromMachineDetail(doc, "最大出メダル"),
      differenceMedal = getDifferenceMedal(graphUrl),
      graphUrl = graphUrl,
      date = today,
    )
  }
  println(machineHistories)

  // 解析して出力
  val differenceMedalsByModel = machineHistories.groupBy(_.machineName).mapValues(_.foldLeft(0)(_ + _.differenceMedal))
  println(differenceMedalsByModel)

  println("End.")

  def requestByGet(url: String) = {
    getConnection(url).get
  }

  def requestByPost(url: String) = {
    getConnection(url).post
  }

  def getConnection(url: String) = {
    Thread.sleep(10000)
    Jsoup.connect(url).userAgent("ps crawler").timeout(30000)
  }

  def getFromMachineDetail(machineDetailDoc: Document, name: String) = {
    val value = machineDetailDoc.select(":containsOwn(" + name + ")").select("p").text()
    value match {
      case "-" => 0
      case _ => value.replace(",", "").toInt // 数字の桁区切りのカンマは削除
    }
  }

  def getDifferenceMedal(graphUrl: String) = {
    val graphImage = ImageIO.read(new URL(graphUrl))
    val rightmostY = (0 until graphImage.getWidth).flatMap { x =>
      (0 until graphImage.getHeight).map { y =>
        val color = graphImage.getRGB(x, y)
        val r = color >> 16 & 0xff
        val g = color >> 8 & 0xff
        val b = color >> 0 & 0xff
        (x, y, r, g, b)
      }
    }.filter(t => t._3 == 236 && t._4 == 34 && t._5 == 52)
    .maxBy(_._1)._2
    (114375 - (375 * rightmostY)) / 9
  }
}


case class MachineHistory(
                           machineNo: Int,
                           machineName: String,
                           bbCount: Int,
                           rbCount: Int,
                           totalStartCount: Int,
                           finalStartCount: Int,
                           maxOutputMedal: Int,
                           differenceMedal: Int,
                           graphUrl: String,
                           date: LocalDate,
                         )