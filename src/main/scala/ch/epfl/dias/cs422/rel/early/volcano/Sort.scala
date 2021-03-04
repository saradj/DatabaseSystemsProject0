package ch.epfl.dias.cs422.rel.early.volcano

import ch.epfl.dias.cs422.helpers.builder.skeleton
import ch.epfl.dias.cs422.helpers.rel.RelOperator.{NilTuple, Tuple}
import org.apache.calcite.rel.RelFieldCollation.Direction
import org.apache.calcite.rel.{RelCollation, RelFieldCollation}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

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

  var table: IndexedSeq[Tuple] = IndexedSeq()
  var index = 0
  var limit = Integer.MAX_VALUE

  /**
    * @inheritdoc
    */
  override def open(): Unit = {
    if (fetch != None) {
      limit = fetch.getOrElse(Integer.MAX_VALUE)
      if (limit == 0) {
        return
      }
    }
    index = 0
    input.open()

    var row = input.next()
    if (row == NilTuple)  {
      table = null
    }
    while (row != NilTuple) {
      table :+= row.get
      row = input.next()
    }

    val field_collations = collation.getFieldCollations

    for (i <- collation.getFieldCollations.size()-1 to 0 by -1) {
      table = table.sortWith((t1, t2) => {
        if (field_collations.get(i).shortString() == "DESC") {
          RelFieldCollation.compare(t1(field_collations.get(i).getFieldIndex).asInstanceOf[Comparable[_]], t2(field_collations.get(i).getFieldIndex).asInstanceOf[Comparable[_]], 0) > 0
        } else {
          RelFieldCollation.compare(t1(field_collations.get(i).getFieldIndex).asInstanceOf[Comparable[_]], t2(field_collations.get(i).getFieldIndex).asInstanceOf[Comparable[_]], 0) < 0
        }
      })
    }

    if (offset != None) {
      index = offset.getOrElse(0)
    }
  }


  /**
    * @inheritdoc
    */
  override def next(): Option[Tuple] ={
    if (table == null || index >= table.size || limit == 0)  {
      return NilTuple
    }
    val row = table(index)
    index += 1
    limit -= 1
    Some(row)
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
