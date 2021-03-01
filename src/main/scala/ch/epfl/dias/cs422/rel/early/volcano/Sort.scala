package ch.epfl.dias.cs422.rel.early.volcano

import ch.epfl.dias.cs422.helpers.builder.skeleton
import ch.epfl.dias.cs422.helpers.rel.RelOperator.{NilTuple, Tuple}
import org.apache.calcite.rel.RelFieldCollation.Direction
import org.apache.calcite.rel.{RelCollation, RelFieldCollation}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable.SortedSet

/**
  * @inheritdoc
  * @see [[ch.epfl.dias.cs422.helpers.builder.skeleton.Sort]]
  * @see [[ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator]]
  */
class Sort protected (
    input: ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator,
    collation: RelCollation,
    offset: Option[Int],
    fetch: Option[Int]
) extends skeleton.Sort[
      ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator
    ](input, collation, offset, fetch)
    with ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator {

  var sorted_input: SortedSet[Sorted] = _
  var iter: Iterator[Sorted] = _

  /**
    * @inheritdoc
    */
  override def open(): Unit = {
    sorted_input = SortedSet()
    for (tuple <- input.iterator) { // .toList.slice(offset.getOrElse(0), offset.getOrElse(0) + fetch.getOrElse(0)) ON THE ITTER to optimize TODO
      sorted_input += Sorted(tuple, collation)
    }
    sorted_input = sorted_input.slice(
      offset.getOrElse(0),
      offset.getOrElse(0) + fetch.getOrElse(Integer.MAX_VALUE))
    iter = sorted_input.iterator

  }

  /**
    * @inheritdoc
    */
  override def next(): Option[Tuple] = {
    var res: Option[Tuple] = NilTuple
    if (iter.hasNext) {
      res = Some(iter.next().getTuple)
    }
    res
  }

  /**
    * @inheritdoc
    */
  override def close(): Unit = {

    input.close()
  }

}
class Sorted(tuple: Tuple, collation: RelCollation) extends Ordered[Sorted] {

  def getTuple: Tuple = tuple
  def getCollation: RelCollation = collation

  def get(i: Int) = tuple(i)

  def compare(that: Sorted): Int = {
    helper_rec(that, collation.getFieldCollations.toSeq)
  }

  def helper_rec(other: Sorted,
                 field_collation_seq: Seq[RelFieldCollation]): Int =
    field_collation_seq match {
      case Seq() => 0
      case Seq(head: RelFieldCollation, tail @ _*) =>
        val idx: Int = head.getFieldIndex
        val (a, b) = (get(idx).asInstanceOf[Comparable[Any]],
                      other.get(idx).asInstanceOf[Comparable[Any]])
        (head.direction match {
          case Direction.ASCENDING | Direction.STRICTLY_ASCENDING =>
            a.compareTo(b)
          case Direction.DESCENDING | Direction.STRICTLY_DESCENDING =>
            b.compareTo(a)
          case _ => throw new Exception("Not a valid sorting direction")
        }) match {
          case 0 => helper_rec(other, tail)
          case r => r
        }
    }
}

object Sorted {
  def apply(tuple: Tuple, collation: RelCollation): Sorted =
    new Sorted(tuple, collation)
}
