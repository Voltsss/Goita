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
    scala.util.Random.shuffle(game.components)

    game.initializePlayer

    val allHuda : ArrayBuffer[Huda] = new ArrayBuffer[Huda]() ++ game.components.collect{case h:Huda => h}
    game.players.foreach(p => for(i<-1 to 8) p.hand += allHuda.remove(0))

    // GameLoop
    // init持ち点、座席
    //// PeriodLoop
    //// init Card配置,初期Starter
    ////// TurnLoop
    ////// Starter移譲
  }
}