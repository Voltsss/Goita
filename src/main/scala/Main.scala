import Model.Component

import scala.collection.mutable.ArrayBuffer

/**
 * Created by volts on 15/11/24.
 */
object Main {
  def main(args:Array[String])={

  }

  def goita()={
    val game = new Goita
    game.componentSetUp

    game.initializePlayer


    // Hand配布

    // GameLoop

    var gameEnd = false
    while(!gameEnd){
      //game.initializePlayer

      var periodEnd = false
      var lastPlayer : Player = scala.util.Random.shuffle(game.players).head

      while(!periodEnd){
        game.initializePeriod

        game.players.foreach(p => p.name + p.hand)

        var turnEnd = false
        while(!turnEnd){
          game.nextTurn.action

          //turnEnd = turn
        }

        periodEnd = true
      }
      gameEnd = true

    }
    // init持ち点、座席
    //// PeriodLoop
    //// init Card配置,初期Starter
    ////// TurnLoop
    ////// Starter移譲

  }
}