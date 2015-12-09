import Model._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by volts on 15/11/25.
 */

sealed abstract class UpDown
case object Up extends UpDown
case object Down extends UpDown

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

case class Player(name:String,game:Goita) extends BasicPlayer{
  val hand : ArrayBuffer[Huda] = new ArrayBuffer[Huda]()
  val playedHuda : ArrayBuffer[Huda] = new ArrayBuffer[Huda]()
  var passed : Boolean = false

  def action: Unit = {
    passed = false
    println("")
    print(name)
    hand.zipWithIndex.foreach{case (h,i)=> print(" #" + i + ":"+ h + ", ")}
    println("")
    println("  *** Play Card (Uke)   : Input Number")
    println("  *** Through   (Nasi)  : Input Else")
    usableHandPrint(hand.toList)
    val r = scala.io.StdIn.readLine()
    r match {
      case "" => through()
      case a:String if (0<=a.toInt & a.toInt<=hand.length-1) => {
        if(game.players.flatMap(_.playedHuda).length <= 0)
          seme(hand.apply(a.toInt))
        else if(lastPlayer.playedHuda.length > 0)
          seme(hand.apply(a.toInt))
        else
          uke(hand.apply(a.toInt))
      }
      case _ => through()
    }
    print("turn end hand")
    hand.zipWithIndex.foreach{case (h,i)=> print(" #" + i + ":"+ h + ", ")}
    println

    def seme(huda:Huda): Unit = {
      if(game.canSeme(game.playerProcession.flatMap(_.playedHuda).toList,huda)) useHand(huda) else through()
    }
    def uke(huda:Huda): Unit = {
      if (game.canUke(lastPlayer.playedHuda.last, huda)) useHand(huda) else through()
    }

  }

  def useHand(huda:Huda): Unit ={
    println("UseHand:"+huda)
    if (game.players.flatMap(_.playedHuda).length <= 0){
      println("ReverseUseHand:(" + huda + ")")

    }else if (game.players.filterNot(_ == this).filter(_.passed == true).length >= 3) {
      println("ReverseUseHand:(" + huda + ")")
    }else{
      println("UseHand:"+huda)
    }
    playedHuda += huda
    hand-=huda
  }

  def through(): Unit ={
    println("through")
    passed = true
  }

  def usableHandPrint(hudas: List[Huda]): Unit = {
    println(" lastPlayer:"+lastPlayer.name)
    val usableHand:List[Huda] = if (lastPlayer.playedHuda.length > 0) {
      println(" lastPlayHuda:" + lastPlayer.playedHuda.last)
      hudas.filter(h => game.canUke(lastPlayer.playedHuda.last,h))
    }else{
      hudas
    }
    print("UsableHand : ")
    usableHand.zipWithIndex.foreach{case (h,i) => print(" #" + i + ":"+ h + ", ")}

    println()
  }

  def lastPlayer: Player ={
    val myIndex :Int = game.playerProcession.zipWithIndex.filter(_._1 == this).head._2
    def lastIndex(i:Int) : Int = {
      val targetIndex : Int = if(i-1 < 0) game.playerProcession.length-1 else i-1
      if(game.players.apply(targetIndex).passed == true) lastIndex(targetIndex) else targetIndex
    }
    game.playerProcession.apply(lastIndex(myIndex))
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
  var turnPass = 0

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
      case gp:GenPlayers => gp.names.foreach(n => players += Player(n,this))
      case gt:GenTeams => //gt.labels.foreach(l => teams += Team(gt,)
      case _ =>
    })

    playerProcession ++= scala.util.Random.shuffle(players)

    genEntities.foreach(g => g match {
      case gts:GenTeams => gts.teams.foreach(gt => teams += Team(gt.label,List(playerProcession.apply(gt.p1),playerProcession.apply(gt.p2))))
      case _ =>
    })

    turnPlayerIndex = 0

    print("Procession:")
    playerProcession.zipWithIndex.foreach{case (p,i) => print("#" + i + ":" + p.name + ", ")}
    println()


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


  def canSeme(baHuda:List[Huda], huda:Huda): Boolean = {
    huda.kind match {
      case Ou =>
        baHuda.filter(_.kind match {case Ou =>true;case _ => false}).length >= 1
      case _  =>
        true
    }
  }

  def canUke(semeHuda:Huda, huda:Huda): Boolean = {
    semeHuda.kind match {
      case Shi => huda.kind match {
        case Shi  => true
        case _    => false
      }
      case Uma => huda.kind match {
        case Uma  => true
        case Ou   => true
        case _    => false
      }
      case Kyo => huda.kind match {
        case Kyo  => true
        case _    => false
      }
      case Gin => huda.kind match {
        case Gin  => true
        case Ou   => true
        case _    => false
      }
      case Kin => huda.kind match {
        case Kin  => true
        case Ou   => true
        case _    => false
      }
      case Hisya => huda.kind match {
        case Hisya  => true
        case Ou     => true
        case _      => false
      }
      case Kaku => huda.kind match {
        case Kaku => true
        case Ou   => true
        case _    => false
      }
      case Ou => huda.kind match {
        case Ou => true
        case _  => false
      }
      case _ => assert(true,"未定義の札が場にあります");false
    }
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
