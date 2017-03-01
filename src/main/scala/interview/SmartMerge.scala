package interview

object SmartMerge extends App {

  val mainInput: List[String] = List("T1","T2","T3","T4","T5","T6")
  val secondaryInput: List[String] = List("TA","T2","TB","TC","T6","TD","T5","TE")

  // Expected Result: T1,TA,T2,T3,T4,TD,T5,TB,TC,T6

  def merge(main: List[String], secondary: List[String]): List[String] = {
    val prefixes = secondaryPrefixes(main.toSet, secondary).withDefaultValue(List.empty)

    mainInput.flatMap(elem => prefixes(Some(elem)) :+ elem) ::: prefixes(None)
  }

  def secondaryPrefixes(main: Set[String], secondary: List[String], acc: Map[Option[String],List[String]] = Map.empty): Map[Option[String],List[String]] = {
    if(secondary.isEmpty) acc
    else {
      val (secondaryElements, remainder) = secondary.span(elem => ! main.contains(elem))
      val anchor = remainder.headOption

      secondaryPrefixes(main,if(remainder.isEmpty) List.empty else remainder.tail,acc + (anchor -> secondaryElements))
    }
  }

  println(secondaryPrefixes(mainInput.toSet,secondaryInput,Map.empty))

  println(merge(mainInput,secondaryInput) == List("T1","TA","T2","T3","T4","TD","T5","TB","TC","T6","TE"))

}
