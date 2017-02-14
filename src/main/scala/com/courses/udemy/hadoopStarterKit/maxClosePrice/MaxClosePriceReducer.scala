package com.courses
package udemy
package hadoopStarterKit
package maxClosePrice


/**
  * MaxClosePriceReducer.java
  * www.hadoopinrealworld.com
  * This is a Reduce program to calculate Max Close Price from stock dataset using MapReduce
  */

import org.apache.hadoop.io.FloatWritable
import org.apache.hadoop.io.Text
import org.apache.hadoop.mapreduce.Reducer


final class MaxClosePriceReducer extends Reducer[Text, FloatWritable, Text, FloatWritable] {

  def reduce(key: Text, values: Iterable[FloatWritable], context: Context): Unit = {

    // danv - it could be done simpler
    // but it will require 2 iterations over the values
    // let us keep it as it is in the original file
    // val maxClosePrice: Float = values.map(_.get).max
    var maxClosePrice: Float = scala.Float.MinValue

    //Iterate all values and calculate maximum
    for (value <- values) {
      maxClosePrice = math.max(maxClosePrice, value.get())
    }

    //Write output
    context.write(key, new FloatWritable(maxClosePrice))
  }

}
