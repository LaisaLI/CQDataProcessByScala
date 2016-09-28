import java.io.{File, PrintWriter}
import java.text.{ParsePosition, SimpleDateFormat}
import java.util.Date

import scala.io.Source

/**
  * Created by Administrator on 2016/9/18.
  * 将gps数据中的时间格式转换为地图匹配代码可识别的格式
  */
object GPSFileTimeTrans {

  val filepath = "./data/GPSFileTimeTrans/"
  val gpsFileName = "chongqing-0301-2.txt"

  def main(args:Array[String]): Unit ={
    splitFile()

  }

  def splitFile(): Unit ={
    val readFile = Source.fromFile(filepath+gpsFileName)
    val writer = new PrintWriter(new File(filepath+"chongqing0301-2.txt"))
    val lines = readFile.getLines()
    while(lines.hasNext){
      val lineArr = lines.next().split(",")
      val timeStr = lineArr(1)
      if(timeStr.length>=20){
        val time = timeStr.substring(0,4)+"/"+timeStr.substring(5,7)+"/"+timeStr.substring(8,10)+"/"+timeStr.substring(11,13)+"/"+timeStr.substring(14,16)+"/"+timeStr.substring(17,19)
        val lineStr = lineArr(0)+","+time+","+lineArr(2)+","+lineArr(3)
        writer.println(lineStr)
      }
      else{
        println(timeStr)
      }

    }

    writer.close()
  }

  //针对重庆的时间数据做的字符串截取原格式：2016/03/01 08:38:52.000000000 转换后的格式："yyyyMMdd HHmmss"
  def strTrans(timeStr:String):String={
    val time =timeStr.substring(0,4)+timeStr.substring(5,7)+timeStr.substring(8,10)+" "+timeStr.substring(11,13)+timeStr.substring(14,16)+timeStr.substring(17,19)
    return time
  }

  //利用java中的时间格式转换函数，将data格式为“yyyyMMdd HHmmss”string转换为Date类型
  def strToDate(timeStr:String):Date={
    val formatter = new SimpleDateFormat("yyyyMMdd HHmmss")
    val pos = new ParsePosition(0);
    val strtodate = formatter.parse(timeStr, pos);
    return strtodate;
  }

}
