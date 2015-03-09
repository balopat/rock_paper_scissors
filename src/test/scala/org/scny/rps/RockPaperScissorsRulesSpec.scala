package org.scny.rps

import org.scalatest.{Matchers, FlatSpec}

class RockPaperScissorsRulesSpec extends FlatSpec with Matchers {

  val COMPUTER: String = "computer"
  val PLAYER: String = "player"
  val DRAW: String = "draw"

  trait Sign {
    def isBeatenBy(sign: Sign): Boolean
  }

  object PAPER extends Sign {
    override def isBeatenBy(sign: Sign) = {
      sign == SCISSORS
    }
  }

  object ROCK extends Sign {
    override def isBeatenBy(sign: Sign) = {
      sign == PAPER
    }
  }
  object SCISSORS extends Sign {
    override def isBeatenBy(sign: Sign) = {
      sign == ROCK
    }
  }


  def getResult(mySign: Sign, computerSign: Sign): String = {
    if (mySign.isBeatenBy(computerSign)) {
      COMPUTER
    } else if (computerSign.isBeatenBy(mySign)) {
      PLAYER
    } else {
      DRAW
    }
  }


  "When I have rock and the computer scissors, then the winner" should "be me" in {
    val mySign = ROCK

    val computersSign = SCISSORS

    val result: String = getResult(mySign, computersSign)
    result should be(PLAYER)
  }

  "When the computer has rock and I have the scissors, then the winner" should "be the computer" in {
    val mySign = SCISSORS

    val computersSign = ROCK

    val result: String = getResult(mySign, computersSign)
    result should be(COMPUTER)
  }


  "When the computer has scissors and I have the paper, then the winner" should "be the computer" in {
    val mySign = PAPER
    val computersSign = SCISSORS

    val result: String = getResult(mySign, computersSign)

    result should be(COMPUTER)
  }



  "When the computer has paper and I have the scissors, then the winner" should "be me" in {
    val mySign = SCISSORS
    val computersSign = PAPER

    val result: String = getResult(mySign, computersSign)

    result should be(PLAYER)
  }


  "When the computer has paper and I have the rock, then the winner" should "be the computer" in {
    val mySign = ROCK
    val computersSign = PAPER

    val result: String = getResult(mySign, computersSign)

    result should be(COMPUTER)
  }

  "When the computer has rock and I have the paper, then the winner" should "be me" in {
    val mySign = PAPER
    val computersSign = ROCK

    val result: String = getResult(mySign, computersSign)

    result should be(PLAYER)
  }


  "When the computer has rock and I have the rock, then the result" should "draw" in {
    val mySign = ROCK
    val computersSign = ROCK

    val result: String = getResult(mySign, computersSign)

    result should be(DRAW)
  }



  "When the computer has paper and I have the paper, then the result" should "draw" in {
    val mySign = PAPER
    val computersSign = PAPER

    val result: String = getResult(mySign, computersSign)

    result should be(DRAW)
  }



  "When the computer has scissors and I have the scissors, then the result" should "scissors" in {
    val mySign = SCISSORS
    val computersSign = SCISSORS

    val result: String = getResult(mySign, computersSign)

    result should be(DRAW)
  }



}
