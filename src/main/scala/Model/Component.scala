

package Model




/**
 * Created by volts on 15/11/25.
 */



abstract class CardState
case object Front extends CardState
case object Back extends CardState

abstract class Component(val label:String) {


}

class Piece(label:String, pieceKind:String) extends Component(label){

}

case class Card(label2: String,cardKind:String) extends Component(label2){
  var state:CardState = Back
  def flip = {println("FLIP!")}
  def isState():CardState = {
    Back
  }

}


