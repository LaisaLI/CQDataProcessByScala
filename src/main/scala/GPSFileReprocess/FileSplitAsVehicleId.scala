import java.io.{File, PrintWriter}

import scala.io.Source

/**
  * Created by Administrator on 2016/9/27.
  * 用于将gps文件和地图匹配得到的轨迹文件按照车辆ID分割成小文件
  *
  */
object FileSplitAsVehicleId {
  val filepath = "./data/FileSplitAsVehicleId/"
  val gpsFileName = "chongqing0301TransforedP1.txt"
  val traceFileName = "chongqing0301TransforedP1withLL.txt"

  def main(args:Array[String]): Unit ={
    splitFile(filepath+traceFileName,filepath+"traceFileSplitAsVehicleId/","")
    splitFile(filepath+gpsFileName,filepath+"GPSFileSplitAsVehicleId/","GPS")

  }

  def splitFile(inputFileName:String,outputPath:String,outputName:String): Unit ={
    val inputFile = Source.fromFile(inputFileName)
    var carId = ""
    val lines = inputFile.getLines()
    var writer :PrintWriter= null
    while( lines.hasNext){
      val line = lines.next()
      val calIdTemp = line.split(",")(0)
      if(!calIdTemp.equals(carId))
        {
          carId = calIdTemp
          writer = new PrintWriter(new File(outputPath+carId+outputName+".txt"))
          writer.println(line)

        }
      else if (writer!=null){
        writer.println(line)
      }
      else{
        println("error")
      }
    }
  }



}
