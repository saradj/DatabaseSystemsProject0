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
class Scan protected(
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

  protected var row_index : Int = 0


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
  override def open(): Unit = row_index=0;
  //{

    //val k =scannable.asInstanceOf[RowStore].getRowCount.asInstanceOf[Int]
    //table_store = IndexedSeq[Tuple]()
    //row_iter=0;

//    if(scannable.asInstanceOf[RowStore].getRowCount.asInstanceOf[Int]>0) {
 //     for (i <- 0 until k) {
   //     table_store = table_store :+ scannable.asInstanceOf[RowStore].getRow(i)
     // }
    //}
    //else row_bool=false;
 // }

  /**
    * @inheritdoc
    */
  override def next(): Option[Tuple] =
  {
  var res: Tuple = null

          if (row_index < scannable.getRowCount)
          {
              res = IndexedSeq[Any]()
              res = scannable.asInstanceOf[RowStore].getRow(row_index)
              row_index += 1
             Some(res)
            }else
           NilTuple


    //}
  }

    /**
    * @inheritdoc
    */
  override def close(): Unit = {
  row_index=0
  }

}
