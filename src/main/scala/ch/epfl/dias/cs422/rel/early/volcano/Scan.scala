package ch.epfl.dias.cs422.rel.early.volcano

import ch.epfl.dias.cs422.helpers.builder.skeleton
import ch.epfl.dias.cs422.helpers.rel.RelOperator.{NilTuple, Tuple}
import ch.epfl.dias.cs422.helpers.store.{RowStore, ScannableTable, Store}
import org.apache.calcite.plan.{RelOptCluster, RelOptTable, RelTraitSet}

/**
  * @inheritdoc
  * @see [[ch.epfl.dias.cs422.helpers.builder.skeleton.Scan]]
  * @see [[ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator]]
  */
class Scan protected (
    cluster: RelOptCluster,
    traitSet: RelTraitSet,
    table: RelOptTable,
    tableToStore: ScannableTable => Store
) extends skeleton.Scan[
      ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator
    ](cluster, traitSet, table)
    with ch.epfl.dias.cs422.helpers.rel.early.volcano.Operator {

  /**
    * A [[Store]] is an in-memory storage the data.
    *
    * Accessing the data is store-type specific and thus
    * you have to convert it to one of the subtypes.
    * See [[getRow]] for an example.
    */
  protected val scannable: Store = tableToStore(
    table.unwrap(classOf[ScannableTable])
  )

  protected var row_index: Int = 0
  protected var storage: IndexedSeq[Tuple] = _
  protected var row_count = 0

  /**
    * Helper function (you do not have to use it or implement it)
    * It's purpose is to show how to convert the [[scannable]] to a
    * specific [[Store]].
    *
    * @param rowId row number (startign from 0)
    * @return the row as a Tuple
    */
  private def getRow(rowId: Int): Tuple = ???

  /**
    * For this project, it's safe to assume scannable will always
    * be a [[RowStore]].
    */
  /**
    * @inheritdoc
    */
  override def open(): Unit = {
    row_index = 0
    storage = IndexedSeq[Tuple]()
    row_count = scannable.getRowCount.asInstanceOf[Int]
    for (i <- 0 until row_count) {
      storage = storage :+ scannable.asInstanceOf[RowStore].getRow(i)
    }
  }

  /**
    * @inheritdoc
    */
  override def next(): Option[Tuple] = {
    var res: Option[Tuple] = NilTuple

    if (row_index < row_count) {
      //res = Some(IndexedSeq[Any]())
      res = Some(storage(row_index))
      row_index += 1
    }
    res

  }

  /**
    * @inheritdoc
    */
  override def close(): Unit = {
    row_index = 0
  }

}
