import Model._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by volts on 15/11/25.
 */
class Goita {
  class GenerateComponent[T<:Component](val label:String,val kind:String,val num:Int)
  val genComponentList:Array[GenerateComponent[Card]] = Array(
    new GenerateComponent[Card]("shi","Koma",10),
    new GenerateComponent[Card]("uma","Koma",4),
    new GenerateComponent[Card]("kyo","Koma",4),
    new GenerateComponent[Card]("gin","Koma",4),
    new GenerateComponent[Card]("kin","Koma",4),
    new GenerateComponent[Card]("hisya","Koma",2),
    new GenerateComponent[Card]("kaku","Koma",2),
    new GenerateComponent[Card]("ou","Koma",2)
  )

  val components:ArrayBuffer[Component] = new ArrayBuffer[Component]()
  def componentSetUp: Unit ={
    genComponentList.foreach(gc => for(i <- 1 to gc.num) components += new Card(gc.label,gc.kind))
  }

  def initializeGame: Unit ={

  }
}
