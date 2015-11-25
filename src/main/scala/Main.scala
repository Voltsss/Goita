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

    // GameLoop
    // init持ち点、座席
    //// PeriodLoop
    //// init Card配置,初期Starter
    ////// TurnLoop
    ////// Starter移譲
  }
}