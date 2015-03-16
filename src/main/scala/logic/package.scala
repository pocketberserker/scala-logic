package object logic {

  import scalaz.Id._

  type Logic[A] = LogicT[Id, A]
}
