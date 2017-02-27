package org.vndzzz.entrytodoc

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.headers.HttpCookie
import org.scalatest._
import org.vndzzz.entrytodoc.dao.JsonConverter
import org.vndzzz.entrytodoc.entity.SpecialityId
import org.vndzzz.entrytodoc.user.WorkFlow.{Empty, NewPolisInfo, ShowSchedule, WorkWithPolis}
import org.vndzzz.entrytodoc.user.{PolisInfo, SelectedItem, UserAction, UserContext}
import com.softwaremill.tagging._

/**
  * Created by vn on 14.12.2016.
  */
class JsonConvertSpec extends FunSpec with Matchers with GivenWhenThen{
  describe("UserContext converter check actions"){
    it("can convert User Context with default values"){
      Given("Default User Context")
      val uc = UserContext()
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same object when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with EnterPolisNumber values") {
      Given("User Context with polisNumber action and no polis number")
      val uc = UserContext(action = UserAction.EnterPolisNumber(None))
      When("convert to string")
      val string =  uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same object when decode")
      uc should equal(UserContext.fromJsonString(string))
    }

    it("can convert User Context with EnterPolisNumber and entered PolisInfo") {
      Given("User Context with EnterPolisNumber action and entered PolisInfo")
      val uc = UserContext(action = UserAction.EnterPolisNumber(Some(PolisInfo(0, "alias", "12345", "01.01.2023"))))
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with EnterDateOfBirth and entered PolisInfo") {
      Given("User Context with EnterDateOfBirth action and entered PolisInfo")
      val uc = UserContext(action = UserAction.EnterDateOfBirth(PolisInfo(0, "alias", "12345", "01.01.2023")))
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with EnterAlias and entered PolisInfo") {
      Given("User Context with EnterAlias action and entered PolisInfo")
      val uc = UserContext(action = UserAction.EnterAlias(PolisInfo(0, "alias", "12345", "01.01.2023")))
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with SubmitRequest and entered PolisInfo") {
      Given("User Context with SubmitRequest action and entered PolisInfo")
      val uc = UserContext(action = UserAction.SubmitRequest(PolisInfo(10, "alias", "12345", "01.01.2023")))
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }


  }

  describe("UserContext convert check workflow"){
    it("can convert User Context with Empty workflow") {
      Given("User Context with ShowSchedule workflow")
      val uc = UserContext(flow = Empty)
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with ShowSchedule workflow") {
      Given("User Context with ShowSchedule workflow")
      val uc = UserContext(flow = ShowSchedule)
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with NewPolisInfo workflow") {
      Given("User Context with NewPolisInfo workflow")
      val uc = UserContext(flow = NewPolisInfo)
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert User Context with WorkWithPolis") {
      Given("User Context with WorkWithPolis workflow")
      val uc = UserContext(flow = WorkWithPolis(PolisInfo(2, "alias", "polisnumber")))
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }
  }

  describe("UserContext convert SelectedTime"){
    it("can convert UserContext with default SelectedItem") {
      Given("UserContext with default SelectedItem")
      val uc = UserContext(selectedItem = SelectedItem())
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

    it("can convert UserContext with SelectedItem filled with some items") {
      Given("UserContext with SelectedItem filled with specId and time")
      val uc = UserContext(selectedItem = SelectedItem(specId = Some("specIdString".taggedWith[SpecialityId]), time = Some("10:10")))
      When("convert to string")
      val string = uc.toJsonString
      info(s"Formatted json: $string")
      Then("receive the same when decode")
      uc should equal (UserContext.fromJsonString(string))
    }

  }

  describe("HttpCookie convert"){
    it("can convert HttpCookie"){
      Given("cookie")
      val cookie = HttpCookie(
        name = "SSESSa8582a3dc976377cc65d7fbc474627fe",
        value = "bS4OVTUgVI-jc3YAlbDflEQZT36tQSq3TjDzBZ5ZtVg",
        maxAge = Some(2000000),
        //25 Dec 2016 13:48:06
        expires = Some(DateTime(2016, 12, 25, 13, 48, 6)),
        domain = Some("uslugi.mosreg.ru"),
        secure = true,
        path = Some("/"),
        httpOnly = true,
        extension = None
      )
      When("convert to string")
      val string = JsonConverter.encode(cookie)
      info(s"Formatted string: $string")
      Then("recive the same when decoded")
      cookie should equal (JsonConverter.decode[HttpCookie](string))
    }
  }

}
