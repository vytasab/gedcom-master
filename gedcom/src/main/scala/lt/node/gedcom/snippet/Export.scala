package lt.node.gedcom.snippet

import java.text.SimpleDateFormat

import _root_.net.liftweb.util.Helpers._
import lt.node.gedcom.model.{Model, Person}
import lt.node.gedcom.rest.GedcomRest
import lt.node.gedcom.util.Utilits
import net.liftweb.common.{Box, Empty}
import net.liftweb.http._
import org.slf4j.{Logger, LoggerFactory}

import scala.xml.Text

//import org.apache.log4j.Level

//import java.util.logging.Level

/**
 * Created by IntelliJ IDEA.
 * User: padargas
 * Date: 1/17/12
 * Time: 5:44 PM
 * To change this template use File | Settings | File Templates.
 * http://chimera.labs.oreilly.com/books/1234000000030/ch02.html#_solution_21
 * https://sysgears.com/articles/implementing-file-download-functionality-in-lift/
 */
  // 18204-7  comment out deprecated code

class Export {

  object personVar extends RequestVar[Box[Person]](Empty)

  val log: Logger = LoggerFactory.getLogger("Export");

  def doAll = {
    GedcomRest.emptyPids
    GedcomRest.emptyFids
    val cdate = new SimpleDateFormat("yyyy-MM-dd_HHmmss").format(System.currentTimeMillis())
    val fileName = "gedcom-"+cdate+".ged"
    //val gedFileTop = Utilits.gedcomHEAD(<_>INDI-0.ged</_>.text)
    val gedFileTop = Utilits.gedcomHEAD(<_>{fileName}</_>.text)
    val gedText = new StringBuffer();
    val ansdesxxx: Tuple3[Int, Int, Boolean] = (S.get("ancestNum").getOrElse("99").toInt,
      S.get("descendNum").getOrElse("99").toInt,
      if (S.get("showSiblings").getOrElse("1") == "1") true else false)
    val persons: List[Person] = Model.createNamedQuery[Person]("findAllPersons").findAll.toList
    for (person <- persons) {
      //GedcomRest.exportPerson(area: Tuple3[Int, Int, Boolean], id: Long, generation: Int, /*jsText*/gedText: StringBuffer, sbIdGen: StringBuffer): Unit = {
      GedcomRest.exportPerson(/*(04,04,true)*/ansdesxxx, person.id, 0, gedText)
    }
    val gedFileMid: String = gedText.toString()
    val gedFile =  gedFileTop + gedFileMid + Utilits.gedcomTRLR()

    def downloadLink4All =
      SHtml.link("/notused", () => throw new ResponseShortcutException(gedFile4All), Text("Download " + fileName) )

    def gedFile4All : LiftResponse =
      InMemoryResponse (
        gedFile.getBytes("UTF-8"),
        "Content-Type" -> "text/plain; charset=utf8" :: "Content-Disposition" -> ("attachment; filename=\""+fileName+"\"") :: Nil,
        cookies=Nil, 200)

    "#gedTitle" #> <span><_>VSH GedCom DB {S.?("export.gedcom")}</_> .ged</span><br/> &
      "a" #> downloadLink4All &
      "#gedFile" #> <pre>{gedFile}</pre>
    //"#gedTitle" #> <span><_>{S.?("export.gedcom")}</_>.text</span> &
    //"#gedFile" #> <pre>{gedFile}</pre>
  }


// 18204-7
// def doPart = {
//    GedcomRest.emptyPids
//    GedcomRest.emptyFids
//    //val person: Option[Person] = Model.find(classOf[Person], S.getSessionAttribute("personId").openOr("1").toLong)
//    val gedFileTop = Utilits.gedcomHEAD(<_>INDI-{S.getSessionAttribute("personId").openOr("ISNOT")}.ged</_>.text)
//    val gedText = new StringBuffer();
//    val gedFileMid: String = (S.getSessionAttribute("personId").openOr("0").toLong) match {
//      case 0L => """"""
//      case _ =>
//        //GedcomRest.exportPerson(area: Tuple3[Int, Int, Boolean], id: Long, generation: Int, /*jsText*/gedText: StringBuffer, sbIdGen: StringBuffer): Unit = {
//        GedcomRest.exportPerson((9.toInt, 9.toInt, true),
//          //(S.get("ancestNum").getOrElse("99").toInt, S.get("descendNum").getOrElse("99").toInt, if (S.get("showSiblings").getOrElse("1") == "1") true else false),
//          S.getSessionAttribute("personId").openOr("0").toLong, 0, gedText)
//        gedText.toString()
//    }
//      val gedFile =  gedFileTop + gedFileMid + Utilits.gedcomTRLR()
//    "#gedTitle" #> <span><_>Person id={S.getSessionAttribute("personId").openOr("ISNOT")} {S.?("export.gedcom")}</_>.text</span> &
//    "#gedFile" #> <pre>{gedFile}</pre>
//  }
//
//
//  def doPartd = {
//    GedcomRest.emptyPids
//    GedcomRest.emptyFids
//    //val person: Option[Person] = Model.find(classOf[Person], S.getSessionAttribute("personId").openOr("1").toLong)
//    val gedFileTop = Utilits.gedcomHEAD(<_>INDI-{S.getSessionAttribute("personId").openOr("ISNOT")}.ged</_>.text)
//    val gedText = new StringBuffer();
//    val gedFileMid: String = (S.getSessionAttribute("personId").openOr("0").toLong) match {
//      case 0L => """"""
//      case _ =>
//        //GedcomRest.exportPerson(area: Tuple3[Int, Int, Boolean], id: Long, generation: Int, /*jsText*/gedText: StringBuffer, sbIdGen: StringBuffer): Unit = {
//        GedcomRest.getPersonGED/*exportPerson*/(/*(04,04,true)*/(S.get("ancestNum").getOrElse("99").toInt,
//          S.get("descendNum").getOrElse("99").toInt,
//          if (S.get("showSiblings").getOrElse("1") == "1") true else false),
//          S.getSessionAttribute("personId").openOr("0").toLong, 0, gedText)
//        gedText.toString()
//    }
//      val gedFile =  gedFileTop + gedFileMid + Utilits.gedcomTRLR()
//      "#gedTitle" #> <span><_>Person id={S.getSessionAttribute("personId").openOr("ISNOT")} {S.?("export.gedcom")}</_>.text</span> &
//      //"#gedFile" #> <pre>{gedFile}</pre>
//      "#gedFile" #> <pre>{gedFile}</pre>
//  }
//
//
//  val gedcomText = doGedcomText
//
//  def doGedcomText: String = {
//    GedcomRest.emptyPids
//    GedcomRest.emptyFids
//    //val person: Option[Person] = Model.find(classOf[Person], S.getSessionAttribute("personId").openOr("1").toLong)
//    val gedFileTop = Utilits.gedcomHEAD(<_>INDI-{S.getSessionAttribute("personId").openOr("ISNOT")}.ged</_>.text)
//    val gedText = new StringBuffer();
//    val gedFileMid: String = (S.getSessionAttribute("personId").openOr("0").toLong) match {
//      case 0L => """"""
//      case _ =>
//        //GedcomRest.exportPerson(area: Tuple3[Int, Int, Boolean], id: Long, generation: Int, /*jsText*/gedText: StringBuffer, sbIdGen: StringBuffer): Unit = {
//        GedcomRest.getPersonGED(/*(04,04,true)*/(S.get("ancestNum").getOrElse("99").toInt,
//          S.get("descendNum").getOrElse("99").toInt,
//          if (S.get("showSiblings").getOrElse("1") == "1") true else false),
//          S.getSessionAttribute("personId").openOr("0").toLong, 0, gedText)
//        gedText.toString()
//    }
//    gedFileTop + gedFileMid + Utilits.gedcomTRLR()
//  }
//
//  val gedTitle: String = "person-"+{S.getSessionAttribute("personId").openOr("ISNOT")}+".ged"
//
//  def render =
//    "#gedTitle" #> <span><_>Person id={S.getSessionAttribute("personId").openOr("ISNOT")} {S.?("export.gedcom")}</_>.text</span> &
//    "a" #> downloadLink &
//    "#gedFile" #> <pre>{gedcomText}</pre>
//
//  def downloadLink =
//    SHtml.link("/notused", () => throw new ResponseShortcutException(gedcomTextFile), Text("Download " + gedTitle) )
//
//  def gedcomTextFile : LiftResponse =
//    InMemoryResponse (
//      gedcomText.getBytes("UTF-8"),
//      "Content-Type" -> "text/plain; charset=utf8" :: "Content-Disposition" -> ("attachment; filename=\""+gedTitle+"\"") :: Nil,
//      cookies=Nil, 200)

}