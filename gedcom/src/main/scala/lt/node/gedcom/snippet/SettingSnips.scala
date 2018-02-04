package lt.node.gedcom.snippet

import _root_.bootstrap.liftweb.{ErrorXmlMsg, Locales}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._  //{ErrorXmlMsg, AccessControl, RequestedURL, CurrentUser}
//import

class SettingSnips {
  val log = Logger("SettingSnips")
  //log.debug(<_>S.locale={S.locale.toString}</_>.text);
  //log.debug(<_>S.param("locale")={S.param("locale")}</_>.text);
  //log.debug(<_>S.get("locale")={S.get("locale").toString}</_>.text);
  log.debug(<_>Locales.langs.size={Locales.langs.size.toString}</_>.text )


  def flag = {
    var selectedLocale: String = S.locale.toString  //.getLanguage
    log.debug(<_>S.uriAndQueryString={S.uriAndQueryString.toString}</_>.text)
    S.get("locale") match{
      case Full(lang) => log.debug(<_>flag Full(lang): |{lang}|</_>.text)
      case Empty =>
        S.set("locale", S.locale.getLanguage.substring(0,2))
        log.debug(<_>flag Empty: |{S.get("locale")}|</_>.text)
      case _ =>
        S.set("locale", "en")
        log.error(<_>flag _ (unexpected case): |{S.get("locale")}|</_>.text)
    }
    val uri = S.uri

    def setLocale2() {
      log.debug(<_>selectedLocale2={selectedLocale}</_>.text)
      S.set("locale", selectedLocale.substring(0,2))
      S.redirectTo(uri)
    }

    //"#lang_lbl" #> S.?("set.locale") &
    "#lang" #> SHtml.select(Locales.LocalesVar.is.toSeq, Full(S.locale.toString),
    {x:String => selectedLocale=x }, "size" ->(Locales.langs.size.toString),
    "onblur" -> "selectWhenChanged(this)", "onchange" -> "selectWhenChanged(this)")  &
    /*"#lang" #> SHtml.radio(Locales.langs/*.toSeq*/, Full(S.locale.toString),{x:String => selectedLocale = x },
    "onblur" -> "selectWhenChanged(this)", "onchange" -> "selectWhenChanged(this)").toForm  &*/
      "#lokale" #> <img src={"/images/flag_" + S.locale.toString.split("_").toList.head.trim + ".png"}
                        title={S.loc("set.locale")} width="4%" height="3%"/> &
      "#submit" #> SHtml.submit("Save", setLocale2)
  }
/*
  val eaCases: List[(String, String)] =
    List(("event", "wiz.event"), ("attrib", "wiz.attribute")).map((kv)=>(kv._1, S ? kv._2))
    val eoaNew = radio(S ? "wiz.add", eaCases.filter((kv) => kv._1==aoeInit).head._2, eaCases.map( _._2),
      valMinLen(1, S ? "wiz.click.radio"))
  val mapGender = Map(S.?("male") -> "M", S.?("female") -> "F")
  SHtml.radio(mapGender.keys.toList, initGender4Radio, {x: String => aGender = mapGender(x)}).toForm

* */

  def ancesDesce = {
    val uri = S.uri
    //    var selectedLocale: String = S.locale.toString  //.getLanguage
    //
    def setAncesDesce() {
      //log.debug(<_>selectedLocale={selectedLocale}</_>.text);
      //S.set("locale", selectedLocale);
      S.redirectTo(uri)
    }

    def checkBoxInnerFunc(boolean: Boolean): Boolean = {
      val x: String = if (boolean) "1" else "0"
      S.set("showPersonDescendAncest", x)
      S.setSessionAttribute("showPersonDescendAncest", x)
      boolean
    }

    //    "#lang_lbl" #> S.?("set.locale") &
    //      "#lang" #> SHtml.select(Locales.LocalesVar.is.toSeq, Full(/*selectedLocale*/S.locale.toString), {
    //        x: String => selectedLocale = x
    //      }, "size" -> (Locales.LocalesVar.is.toSeq.size.toString)) &
    //      //"#domain" #> SHtml.radio(Locales.LocalesVar.is.keys.toList, Full(selectedLocale),
    //      //  {x: String => selectedLocale = Locales.LocalesVar.is.apply(x)},
    //      //  "onclick" -> ).toForm  &
    val title = S.loc("set.forest.params")
    log.debug(<_>SettingSnips S.getSessionAttribute("showPersonDescendAncest")={S.getSessionAttribute("showPersonDescendAncest").toString}</_>.text);
    log.debug(<_>SettingSnips S.get("showPersonDescendAncest")={S.get("showPersonDescendAncest").toString}</_>.text);
    // var showPersonDescendAncest = if (S.get("showPersonDescendAncest").openOr("1")=="1") true else false
    var showPersonDescendAncest = if (S.getSessionAttribute("showPersonDescendAncest").openOr("0")=="1") true else false
    S.session match {
      case Full(sess) =>
        val options = S.locale.getLanguage match {
          case "lt" => <_>Protėvių: {S.get("ancestNum").getOrElse("1")} karta(os), palikuonių: {S.get("descendNum").getOrElse("1")} karta(os); </_>.text +
            <_>{if (S.get("showPersonDescendAncest").getOrElse("0")=="1") S.?("set.spda") else ""+S.?("set.spdano") }</_>.text
            //(if (S.get("showPersonDescendAncest").openOr("1")=="1") S.?("set.spda") else <_>nerodyti</_>.text)
          case "en" => <_>Ancestors: {S.get("ancestNum").getOrElse("1")} generation(s), descendants: {S.get("descendNum").getOrElse("1")} generation(s); </_>.text +
            <_>{if (S.get("showPersonDescendAncest").getOrElse("0")=="1") S.?("set.spda") else ""+S.?("set.spdano") }</_>.text
          case _ => ""
        }
        "#forestSettingsShow" #> <span style="display:yes" title={S.loc("set.forest.params")}>{options}</span> &
          "#ancest_lbl" #> <span style="display:none">{S.?("set.ancestor.num")}</span>  &
          "#ancest" #> SHtml.select((1 to 6).toList.map{x => (<_>{x}</_>.text, <_>{x}</_>.text)}.toMap.toSeq,
            Full(S.get("ancestNum").getOrElse("1")), S.set("ancestNum", _),
            "size" -> "6", "onchange" -> "selectWhenChangedAncesDesce(this)", "onmouseover" -> "onMouseOver()" ) &
          "#descend_lbl" #> <span style="display:none">{S.?("set.descendant.num")}</span>  &
          "#descend" #> SHtml.select((1 to 6).toList.map{x => (<_>{x}</_>.text, <_>{x}</_>.text)}.toMap.toSeq,
            Full(S.get("descendNum").getOrElse("1")), S.set("descendNum", _),
            "size" -> "6", "onchange" -> "selectWhenChangedAncesDesce(this)") &
          "#spda_lbl" #> <span style="display:none">{S.?("set.spda_lbl")}</span> &
          "#spda" #> SHtml.checkbox(showPersonDescendAncest, /*showPersonDescendAncest =*/ checkBoxInnerFunc(_),
            "onchange" -> "selectWhenChangedAncesDesce(this)" ) &
          //"#sibl_lbl" #> S.?("set.show.siblings") &
          //"#sibl" #> SHtml.checkbox(/*true*/(S.get("showSiblings").openOr("1")=="1"), {
          //   x: Boolean => S.set("showSiblings", (if (x) "1" else "0") )
          //}) &
          "#submit2" #> SHtml.submit("Save", setAncesDesce)
/*, ("id", "spda")*/
      case Empty =>
        val msg = "forestShowOptions: Lift session is over go to Settings"
        log.warn(msg)
        S.redirectTo("/errorPage", () => {
          ErrorXmlMsg.set(Some(Map(
            "location" -> <p>SettingSnips.forestShowOptions</p>,
            "message" -> <p>{msg}</p>)))
        })
      case Failure(msg, _, _) =>
        log.warn(msg)
        S.redirectTo("/errorPage", () => {
          ErrorXmlMsg.set(Some(Map(
            "location" -> <p>SettingSnips.forestShowOptions</p>,
            "message" -> <p>{msg}</p>)))
        })
      case _ =>
        val msg = "forestShowOptions: other reson"
        log.warn(msg)
        S.redirectTo("/errorPage", () => {
          ErrorXmlMsg.set(Some(Map(
            "location" -> <p>SettingSnips.forestShowOptions</p>,
            "message" -> <p>{msg}</p>)))
        })
    }
  }


  def forestShowOptions = {
    S.session match {
      case Full(sess) =>
        val options = S.locale.getLanguage match {
          case "lt" => <_>Rodoma protėvių: {S.get("ancestNum").getOrElse("1")} karta(os), palikuonių: {S.get("descendNum").getOrElse("1")} karta(os)</_>.text +
            <_>{if (S.get("showPersonDescendAncest").getOrElse("1")=="1") S.?("set.spda") else "ne"+S.?("set.spda") } "mmmm"</_>.text
            //{if (S.get("showSiblings").getOrElse("1")=="1") "su broliais/seserimis" else "be brolių/seserų" }</_>.text
          case "en" => <_>Display ancestors: {S.get("ancestNum").getOrElse("1")} generation(s), descendant: {S.get("descendNum").getOrElse("1")}  generation(s) {if (S.get("showSiblings").getOrElse("1")=="1") "with" else "without"} siblings</_>.text
          case _ => ""
        }
        "#forestShowOptions" #> options
      case Empty =>
        val msg = "forestShowOptions: Lift session is over go to \"Settings\""
        log.warn(msg)
        S.redirectTo("/errorPage", () => {
          ErrorXmlMsg.set(Some(Map(
            "location" -> <p>SettingSnips.forestShowOptions</p>,
            "message" -> <p>{msg}</p>)))
        })
      case Failure(msg, _, _) =>
        log.warn(msg)
        S.redirectTo("/errorPage", () => {
          ErrorXmlMsg.set(Some(Map(
            "location" -> <p>SettingSnips.forestShowOptions</p>,
            "message" -> <p>{msg}</p>)))
        })
      case _ =>
    }
  }

  //  def list = {
//    var selectedLocale: String = S.locale.toString  //.getLanguage
//
//    def setLocale(): Unit = {
//      log.debug(<_>selectedLocale={selectedLocale}</_>.text);
//      S.set("locale", selectedLocale);
//      S.redirectTo("/");
//    };
//
//    "#lang_lbl" #> S.?("set.locale") &
//      "#lang" #> SHtml.select(Locales.LocalesVar.is.toSeq, Full(/*selectedLocale*/S.locale.toString), {
//        x: String => selectedLocale = x
//      }, "size" -> (Locales.LocalesVar.is.toSeq.size.toString)) &
//      //"#domain" #> SHtml.radio(Locales.LocalesVar.is.keys.toList, Full(selectedLocale),
//      //  {x: String => selectedLocale = Locales.LocalesVar.is.apply(x)},
//      //  "onclick" -> ).toForm  &
//      "#ancest_lbl" #> S.?("set.ancestor.num") &
//      "#ancest" #> SHtml.select((1 to 4).toList.map{
//        x => (<_>{x}</_>.text, <_>{x}</_>.text)
//      }.toMap.toSeq,
//        Full(S.get("ancestNum").getOrElse("1")), S.set("ancestNum", _), "size" -> (1 to 4).size.toString) &
//      "#descend_lbl" #> S.?("set.descendant.num") &
//      "#descend" #> SHtml.select((1 to 4).toList.map{
//        x => (<_>{x}</_>.text, <_>{x}</_>.text)
//      }.toMap.toSeq,
//        Full(S.get("descendNum").getOrElse("1")), S.set("descendNum", _), "size" -> (1 to 4).size.toString) &
//      "#sibl_lbl" #> S.?("set.show.siblings") &
//      "#sibl" #> SHtml.checkbox(/*true*/(S.get("showSiblings").openOr("1")=="1"), {
//         x: Boolean => S.set("showSiblings", (if (x) "1" else "0") )
//      }) &
//      "#submit" #> SHtml.submit("Save", setLocale)
//  }


/*
  def ancest/*Setting*/ = {
    //var selectedLocale: String = S.locale.toString  //.getLanguage
    //log.debug(<_>S.uriAndQueryString={S.uriAndQueryString.toString}</_>.text);
    val uri = S.uri;

    def setLocale2(): Unit = {
      log.debug(<_>selectedLocale2={selectedLocale}</_>.text);
      S.set("locale", selectedLocale);
      S.redirectTo(uri);
    };

//    "#ancest_lbl" #> S.?("set.ancestor.num") &
    "#ancest" #> SHtml.select((1 to 4).toList.map{x => (<_>{x}</_>.text, <_>{x}</_>.text)}.toMap.toSeq,
      Full(S.get("ancestNum").getOrElse("2")), S.set("ancestNum", _), "size" -> (1 to 4).size.toString) &

//    "#lang_lbl" #> S.?("set.locale") &
      "#lang" #> SHtml.select(Locales.LocalesVar.is.toSeq, Full(S.locale.toString),
        { x: String => selectedLocale = x }, "size" -> "1"/*(Locales.LocalesVar.is.toSeq.size.toString)*/,
        "onchange" -> "selectWhenChanged(this)")  &
      "#lokale *" #> <img src={"/images/flag_" + S.locale.toString.split("_").toList.head.trim + ".png"} width="4%" height="3%"/> &
      "#submit" #> SHtml.submit("Save", setLocale2)
  }
*/

      //"#domain" #> SHtml.radio(Locales.LocalesVar.is.keys.toList, Full(selectedLocale),
      //  {x: String => selectedLocale = Locales.LocalesVar.is.apply(x)},
      //  "onclick" -> ).toForm  &


/*
  def list /*(xhtml: Group): NodeSeq*/ = {
    val items = Model.createNamedQuery[Person]("findAllPersons").getResultList()
    var personList = items.map(i => ((/*"/rest/person/" + */ i.id.toString),
      ((if (i.gender == "M") "♀" else "\u2642") + " " + i.nameGivn + " " + i.nameSurn)
      )
    ).toList;
    personList = ("", "-- " + S.?("select.person") + " --") :: personList

    //log.debug(items.size.toString)
    var selectedPersonURL: String = ""

    def preparePersonData(): Unit = {
      log.debug(<_>selectedPersonURL={selectedPersonURL}</_>.text);
      S.redirectTo(<_>/rest/person/{selectedPersonURL}</_>.text)
    }

    "#domain" #> FocusOnLoad(SHtml.select(personList, Empty,
      { selectedPersonURL = _ },
      "size" -> "1", "onchange" -> "selectWhenChanged(this)" ) ) &
      "#submit" #> SHtml.submit("Save", preparePersonData)
  }
 */



//      "#ancest_lbl" #> S.?("set.ancestor.num") &
//      "#ancest" #> SHtml.select((1 to 4).toList.map{
//        x => (<_>{x}</_>.text, <_>{x}</_>.text)
//      }.toMap.toSeq,
//        Full(S.get("ancestNum").getOrElse("2")), S.set("ancestNum", _), "size" -> (1 to 4).size.toString) &
//      "#descend_lbl" #> S.?("set.descendant.num") &
//      "#descend" #> SHtml.select((1 to 4).toList.map{
//        x => (<_>{x}</_>.text, <_>{x}</_>.text)
//      }.toMap.toSeq,
//        Full(S.get("descendNum").getOrElse("2")), S.set("descendNum", _), "size" -> (1 to 4).size.toString) &
//      "#sibl_lbl" #> S.?("set.show.siblings") &
//      "#sibl" #> SHtml.checkbox(/*true(S.get("showSiblings").openOr("1")=="1"), {
//         x: Boolean => S.set("showSiblings", (if (x) "1" else "0") )
//      }) &


//  def flag = {
//    val lokale = "/images/flag_" + S.locale.toString.split("_").toList.head.trim + ".png"
//    "#lokale *" #> <img src={lokale} width="4%" height="4%"/>
//  }

}
