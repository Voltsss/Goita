package Model


/**
 * Created by volts on 15/11/25.
 */



abstract class CardState
case object Front extends CardState
case object Back extends CardState

sealed abstract class Component

abstract class Piece extends Component
abstract class Card extends Component

// 生成コンポーネント型
class GenComponent