package com.courses
package udemy
package hadoopStarterKit
package maxClosePrice


/**
  * MaxClosePrice.java
  * www.hadoopinrealworld.com
  * This is a driver program to calculate Max Close Price from stock dataset using MapReduce
  */

import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.FloatWritable
import org.apache.hadoop.io.Text
import org.apache.hadoop.mapreduce.Job
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat


final class MaxClosePrice


object MaxClosePrice {

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      System.err.println("Usage: MaxClosePrice <input path> <output path>")
      System.exit(-1)
    }

    //Define MapReduce job
    var job: Job = new Job()
    job.setJarByClass(classOf[MaxClosePrice])
    job.setJobName("MaxClosePrice")

    //Set input and output locations
    FileInputFormat.addInputPath(job, new Path(args(0)))
    FileOutputFormat.setOutputPath(job, new Path(args(1)))

    //Set Input and Output formats
    job.setInputFormatClass(classOf[TextInputFormat])
    job.setOutputFormatClass(classOf[TextOutputFormat[Text, FloatWritable]])

    //Set Mapper and Reduce classes
    job.setMapperClass(classOf[MaxClosePriceMapper])
    job.setReducerClass(classOf[MaxClosePriceReducer])


    //Output types
    job.setOutputKeyClass(classOf[Text])
    job.setOutputValueClass(classOf[FloatWritable])

    //Submit job
    val exitVal: Int = if (job.waitForCompletion(true)) 0 else 1
    System.exit(exitVal)
  }

}
