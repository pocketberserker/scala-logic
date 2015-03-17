package logic

import org.scalacheck._

abstract class SpecLite extends Properties("") {

  def checkAll(props: Properties) {
    for ((name, prop) <- props.properties) yield {
      property(name) = prop
    }
  }
}
