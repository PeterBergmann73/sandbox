package com.courses
package udemy
package hadoopStarterKit
package maxClosePrice


/**
  * MaxClosePriceMapper.java
  * www.hadoopinrealworld.com
  * This is a Mapper program to calculate Max Close Price from stock dataset using MapReduce
  */

import org.apache.hadoop.io.FloatWritable
import org.apache.hadoop.io.LongWritable
import org.apache.hadoop.io.Text
import org.apache.hadoop.mapreduce.Mapper


final class MaxClosePriceMapper extends Mapper[LongWritable, Text, Text, FloatWritable] {

  override def map(key: LongWritable,
                   value: Text,
                   context: Mapper[LongWritable, Text, Text, FloatWritable]#Context): Unit = {
    val line: String = value.toString
    val items: Array[String] = line.split(",")

    val stock: String = items(1)
    val closePrice: Float = items(6).toFloat

    context.write(new Text(stock), new FloatWritable(closePrice))
  }
}
