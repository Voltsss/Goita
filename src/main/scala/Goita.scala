import Model._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by volts on 15/11/25.
 */

// 札の種類
sealed abstract class HudaType
case object Shi extends HudaType
case object Uma extends HudaType
case object Kyo extends HudaType
case object Gin extends HudaType
case object Kin extends HudaType
case object Hisya extends HudaType
case object Kaku extends HudaType
case object Ou extends HudaType

// 札クラス
case object Huda extends Card
case class Huda(kind:HudaType) extends Card

// 生成コンポーネント型
case class GenHuda (h:HudaType,num:Int) extends GenComponent

case class GenTeam(label:String,p1:Int,p2:Int)

case class Player(name:String) extends BasicPlayer{
  val hand : ArrayBuffer[Huda] = new ArrayBuffer[Huda]()

  def action: Unit = {
    print(name)
    hand.zipWithIndex.foreach{case (h,i)=> print(" #" + i + ":"+ h + ", ")}
    println("")
    println("*** Play Card (Uke)   : Input Number")
    println("*** Through   (Nasi)  : Input Else")
    val r = scala.io.StdIn.readLine()
    r match {
      case "" => println("through")
      case a:String => if (0<=a.toInt&a.toInt<=hand.length-1){
        println("input "+a)
        useHand(hand.apply(a.toInt))
      }else {
        println("through")
      }
    }
    print("turn end hand")
    hand.zipWithIndex.foreach{case (h,i)=> print(" #" + i + ":"+ h + ", ")}
    println
  }

  def useHand(huda:Huda): Unit ={
    println("UseHand:"+huda)
    hand-=huda
  }

}
case class Team(label:String,p:List[Player],var score:Int=0) extends BasicPlayer

abstract class GenEntiry
case class GenPlayers(names:List[String]) extends GenEntiry
case class GenTeams(teams:List[GenTeam]) extends GenEntiry



class Goita {
  // 生成コンポーネント
  val genCompList :List[GenComponent] = List(
    GenHuda(Shi,10),
    GenHuda(Uma,4),
    GenHuda(Kyo,4),
    GenHuda(Gin,4),
    GenHuda(Kin,4),
    GenHuda(Hisya,2),
    GenHuda(Kaku,2),
    GenHuda(Ou,2)
  )
  val genPlayerName : List[String] = List("Player1","Player2","Player3","Player4")
  val genTeam : List[GenTeam] = List(GenTeam("Team1&3",0,2),GenTeam("Team2&4",1,3))
  val genEntities : List[GenEntiry] = List(
    GenPlayers(genPlayerName),
    GenTeams(genTeam)
  )

  val components:ArrayBuffer[Component] = new ArrayBuffer[Component]()
  val players:ArrayBuffer[Player] = new ArrayBuffer[Player]()
  val teams:ArrayBuffer[Team] = new ArrayBuffer[Team]()

  var finishedPeriod = false
  val playerProcession:ArrayBuffer[Player] = new ArrayBuffer[Player]()
  var turnPlayerIndex = 0

  def componentSetUp: Unit ={
    genCompList.foreach(g => g match {
      case gh:GenHuda => gh.h match {
        case ht:HudaType => for(i <- 1 to gh.num) components += Huda(ht)
        case _ =>
      }
      case _ =>
    })
  }

  def initializePlayer: Unit ={
    players.clear()
    teams.clear()
    playerProcession.clear()

    genEntities.foreach(g => g match {
      case gp:GenPlayers => gp.names.foreach(n => players += Player(n))
      case gt:GenTeams => //gt.labels.foreach(l => teams += Team(gt,)
      case _ =>
    })

    playerProcession ++= scala.util.Random.shuffle(players)

    genEntities.foreach(g => g match {
      case gts:GenTeams => gts.teams.foreach(gt => teams += Team(gt.label,List(playerProcession.apply(gt.p1),playerProcession.apply(gt.p2))))
      case _ =>
    })

    turnPlayerIndex = 0


  }

  def initializePeriod: Unit ={
    //teams.map(_.score=0)
    dealHand
    turnPlayerIndex = 0
  }
  def dealHand: Unit ={
    val allHuda : ArrayBuffer[Huda] = new ArrayBuffer[Huda]() ++ components.collect{case h:Huda => h}
    scala.util.Random.shuffle(allHuda)
    players.foreach(_.hand.clear())
    players.foreach(p => for(i<-1 to 8) p.hand += allHuda.remove(0))
  }
  


  
  def hasNextTurn : Boolean = {
    !finishedPeriod
  }

  def nextTurn:Player = {
    val p = playerProcession.apply(turnPlayerIndex)
    if(turnPlayerIndex < playerProcession.length -1) turnPlayerIndex += 1 else turnPlayerIndex = 0
    return p
  }

  
}
