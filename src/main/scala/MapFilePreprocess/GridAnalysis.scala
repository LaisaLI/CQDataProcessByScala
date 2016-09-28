import java.io.{File, PrintWriter}

import scala.Array._
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by Lasia on 2016/8/2.
  */
object GridAnalysis {
  val cityName = "chongqing"
  val filePath = "E:\\coding\\lab\\scala\\data\\"+cityName+"\\"
  val inputMif = filePath+"R"+cityName+".mif"
  val inputMid = filePath+"R"+cityName+".mid"
  val resultLinkNodeRead = filePath+"snodeNenode.txt"
  val resultMapIdMatch = filePath+"mapIdminNmax.txt"
  val resultAnalyseMapId = filePath+cityName+"Grid.txt"
  val resultgenerateMapSnodeId = filePath+cityName+"NextLineResult.txt"
  val resultgenerateMapSnodeIdWithLonNLat = filePath+cityName+"NextLineResultWithLonNLat.txt"
  val resultgenerateLinkIdWithLonNLat = filePath+cityName+"LinkIdWithLonNLat.txt"
  val resultgenerateMapSnodeIdIndex = filePath+cityName+"NextLineIndex.txt"
  val resultnodeIdMathLonNLat = filePath+cityName+"NodeIdWithLonNLat.txt"






  var lonMin = 116.625//最小经度值
  var triLatMin = 115.5//最小纬度值*3
  var lonMax = 0.0 //最大经度值
  var triLatMax = 0.0 //最大纬度值*3

  def main(args:Array[String]){
//    linkNodeRead()
//    mapIdMatch()
//    analyseMapId()
//    generateMapSnodeId()
//    generateMapSnodeIdIndex()
//    gpsDataProcess()

//    generateMapSnodeIdWithLonNLat()

//    nodeIdMathLonNLat()
    generateLinkIdWithLonNLat()
  }

//该函数用于将mif中每条链路的起始节点提取
  def linkNodeRead(): Unit ={
//    val mapFile =Source.fromFile("E:\\coding\\lab\\scala\\data\\Rchongqing.mif")
    val mapFile =Source.fromFile(inputMif)

//  val writer = new PrintWriter(new File("test.txt" ))
  val writer = new PrintWriter(new File(resultLinkNodeRead))
    val lines = mapFile.getLines()
    while(lines.hasNext){
      val line = lines.next()
      val lineArray = line.split(" ")
      //lineArray.foreach(println(_))
      lineArray(0) match{
        case "Pline"=>{
          val  number = lineArray(1).toInt
          var gpsSNode = ""
          var gpsENode = ""
          for(i<- 1 to number){
            if(i==1){
              val gps = lines.next().split(" ")
              //gps.foreach(println(_))

              gpsSNode=gps(0)+","+gps(1)
            }

            else if(i == number){
              val gps = lines.next().split(" ")
              //gps.foreach(println(_))
              gpsENode=gps(0)+","+gps(1)

//              gpsNodeList(2) = gps(0).toDouble
//              gpsNodeList(3) = gps(1).toDouble
            }
            else lines.next()
          }

          println(gpsSNode+","+gpsENode)
          writer.println(gpsSNode+","+gpsENode)



        }
        case "Line"=>{
          if(lineArray.size==5){
            val gpsNodeLine = lineArray(1)+","+lineArray(2)+","+lineArray(3)+","+lineArray(4)
            writer.println(gpsNodeLine)
          }

          else{
            println("Line's gpsNode is Wrong!")
          }


        }
        case _ =>{

        }


      }
    }

    writer.close()

  }

  //用于将mapIdRead提取出来的链路信息与mid中mapId对应，并将每个mapId中的经纬度最大值和最小值保存到resultMapIdMatch中
  def mapIdMatch(): Unit ={

//    val mapFile1 =Source.fromFile("E:\\coding\\lab\\scala\\data\\MidChongqingNodeResult.txt")
    val mapFile1 =Source.fromFile(resultLinkNodeRead)
//    val mapFile2 =Source.fromFile("E:\\coding\\lab\\scala\\data\\Rchongqing.mid")
    val mapFile2 =Source.fromFile(inputMid)

    if(mapFile1 == null){println("1wrong!")}
    if(mapFile2 == null){println("2wrong!")}
    val writer = new PrintWriter(new File(resultMapIdMatch ))
//    val lines1 = mapFile1.getLines().toList.map(_.split(","))
//    val lines2 = mapFile2.getLines().toList.map(_.split(",")(0).substring(1,7))
    val lines1 = mapFile1.getLines()
    val lines2 = mapFile2.getLines()
//    mapFile1.getLines().foreach(println(_))
    var mapIdPostionMap:Map[String,Array[Double]] = Map()
//    if(lines1.size==0 || lines2.size == 0){
//      println("lines's size is 0!")
//    }
//    println(lines1.size+" "+lines2.size)
//    if(lines1.size == lines2.size ){
//      val isTemp =false
  while(lines1.hasNext && lines2.hasNext)
//      for(i<- 0 to lines1.size-1)
        {

          val position = lines1.next().split(",")
//          val position = lines1(i)
          val jdTemp = Set(position(0).toDouble,position(2).toDouble)
          val wdTemp = Set(position(1).toDouble,position(3).toDouble)
          val mapId = lines2.next().split(",")(0).substring(1,7)
//          val mapId = lines2(i)
//          if(isTemp==false)
//          {
//            position.foreach(println(_))
//            println(mapId)
//
//          }
          if(mapIdPostionMap.contains(mapId)){
            val currentPosition = mapIdPostionMap(mapId)
            var isChanged = false
            val positionJdMin =if(jdTemp.min<currentPosition(0)){isChanged = true;jdTemp.min}else{currentPosition(0)}
            val positionJdMax =if(jdTemp.max>currentPosition(2)){isChanged = true;jdTemp.max}else{currentPosition(2)}
            val positionWdMin =if(wdTemp.min<currentPosition(1)){isChanged = true;wdTemp.min}else{currentPosition(1)}
            val positionWdMax =if(wdTemp.max>currentPosition(3)){isChanged = true;wdTemp.max}else{currentPosition(3)}
            if(isChanged)
              {
                mapIdPostionMap.updated(mapId,Array(positionJdMin,positionWdMin,positionJdMax,positionWdMax))
              }


          }
          mapIdPostionMap += (mapId->Array(jdTemp.min,wdTemp.min,jdTemp.max,wdTemp.max))
        }
      if(mapIdPostionMap.size!=0)
        {
          mapIdPostionMap.map(x=>{writer.print(x._1);x._2.foreach(x=>writer.print(","+x));writer.print("\n")})

        }
      else{
        println("mapIdPosition's size is 0!")
      }
      writer.close()
//    }
//
//    else{
//      println("file1's size != file2's size1")
//    }


  }


  //分析mapId的经纬度后建立对应的grid数组存储mapid的位置关系，存入chongqingGrid.txt
  def analyseMapId(): Unit ={
//    val mapFile1 =Source.fromFile("E:\\coding\\lab\\scala\\data\\mapId.csv")
    val mapFile1 =Source.fromFile(resultMapIdMatch)
    //调用该函数为最小经度和最小纬度赋值
    getMinNMaxLonNLat()
    println(lonMin+","+triLatMin+";"+lonMax+","+triLatMax)
    val lonSize = ((lonMax-lonMin)/0.125).toInt+1
    val latSize = ((triLatMax-triLatMin)/0.25).toInt+1

    println(lonSize+","+latSize)

    var gridArray = ofDim[String](latSize,lonSize)
    for(i <- 0 to latSize-1){
      for(j <- 0 to lonSize-1){
        gridArray(i)(j)="0"
      }
    }


    if(mapFile1 == null){println("1wrong!")}
    val writer = new PrintWriter(new File(resultAnalyseMapId ))
    val lines = mapFile1.getLines()
    while(lines.hasNext){
      val line = lines.next().split(",")
      val mapId = line(0)
      val positionArray = geographicGrid(mapId)


      if(positionArray!= null)
        {
          val girdIndexX = ((positionArray(0)-lonMin)/0.125).toInt
          val gridIndexY = ((positionArray(1)-triLatMin)/0.25).toInt
          println(girdIndexX+","+gridIndexY)
          if(girdIndexX<lonSize && gridIndexY < latSize && gridArray(gridIndexY)(girdIndexX).equals("0")){
            gridArray(gridIndexY)(girdIndexX)=mapId
          }
          else{
            println(mapId+"'s grid number is wrong!")
          }

//            if(positionArray(0)<=line(1).toDouble&&positionArray(1)<=line(2).toDouble)
//            {
//              writer.println(mapId+","+positionArray(0)+","+positionArray(1))
//            }
//          else{
//            println(mapId+"'s positionArray is wrong!")
//          }

        }
      else{
        println(mapId+"'s position array is null!")
      }


    }
    writer.print("{\n")
    for(i <- 0 to latSize-1){
      writer.print("{")
      for(j <- 0 to lonSize-2){
        writer.print(gridArray(i)(j)+",")
      }
      writer.print(gridArray(i)(lonSize-1)+"},\n")
    }
    writer.print("}")

  writer.close()

  }
//通过分析mapId得到本地图的最大、小经度和最大、小纬度
  def getMinNMaxLonNLat(): Unit ={
    //    val mapFile1 =Source.fromFile("E:\\coding\\lab\\scala\\data\\mapId.csv")
    val mapFile1 =Source.fromFile(resultMapIdMatch)
    var gridArray = ofDim[String](50,40)
    for(i <- 0 to 49){
      for(j <- 0 to 39){
        gridArray(i)(j)="0"
      }
    }
    if(mapFile1 == null){println("1wrong!")}
    val writer = new PrintWriter(new File(resultAnalyseMapId ))
    val lines = mapFile1.getLines()
    var templonMin = 10000.0
    var templatMin = 10000.0
    var templonMax = 0.0
    var templatMax = 0.0
    while(lines.hasNext){
      val line = lines.next().split(",")
      val mapId = line(0)
      val positionArray = geographicGrid(mapId)
      if(positionArray(0)<templonMin){
        templonMin = positionArray(0)
      }
      if(positionArray(1)<templatMin){
        templatMin = positionArray(1)
      }
      if(positionArray(0)>templonMax){
        templonMax = positionArray(0)
      }
      if(positionArray(1)>templatMax){
        templatMax = positionArray(1)
      }
    }
    lonMin = templonMin
    triLatMin = templatMin
    lonMax = templonMax
    triLatMax =templatMax

  }

  //传入mapid来倒推其对应的左下角经纬度，纬度lat返回的值为真实纬度值*3的结果
  def geographicGrid(code:String ): Array[Double]={
    val geographicCode = code;
    if (code.length() == 6) {
      val x1:Int = code.charAt(5) - '0'
      val y1:Int = code.charAt(4) - '0'
      val lonCode = code.substring(2, 4).toDouble
      val latCode = code.substring(0, 2).toDouble
      val lon = lonCode+60.0 + 0.125 * x1 ;
      val lat = (latCode +0.125 * y1 ) * 2.0;
      Array(lon,lat)
    }
    else{
      println("mapId's length is wrong!")
      null
    }
  }

  //得到path1和path2文件
//  def generateMapSnodeId(): Unit ={
//    val mapFile1 =Source.fromFile("E:\\coding\\lab\\scala\\data\\Rchongqing.mid")
//
//    val writer = new PrintWriter(new File("chongqingNextLineResult.txt" ))
//
//    val lines = mapFile1.getLines()
//    var mapIdList = ListBuffer[Array[String]]()
//
//    while(lines.hasNext){
//      val lineArray  = lines.next().split(",")
//      val mapId = lineArray(0).substring(1,lineArray(0).length-1)
//      val direction = lineArray(5).substring(1,lineArray(5).length-1)
//      val snodeId = lineArray(9).substring(1,lineArray(9).length-1)
//      val enodeId = lineArray(10).substring(1,lineArray(10).length-1)
//      val linkId = lineArray(1).substring(1,lineArray(1).length-1)
//      mapIdList.+=(Array(mapId,snodeId,direction,linkId))
//      mapIdList.+=(Array(mapId,enodeId,direction,linkId))
//
//    }
//
//    val mapIdSortedList = mapIdList.sortWith((x1,x2)=>{(x1(0)<x2(0))||(x1(0)==x2(0)&&x1(1)<x2(1))})
//
//    mapIdSortedList.foreach(x=>{if(x.size==4) writer.print(x(0)+","+x(1)+","+x(2)+","+x(3)+"\n");else{ println("wrong!")}})
//
//
//
//
//  }

  //NextLineResult文件生成
  def generateMapSnodeId(): Unit ={
    val mapFile1 =Source.fromFile(inputMid)

    val writer = new PrintWriter(new File(resultgenerateMapSnodeId ))

    val lines = mapFile1.getLines()
    var mapIdList = ListBuffer[Array[String]]()

    while(lines.hasNext){
      val lineArray  = lines.next().split(",")
      val mapId = lineArray(0).substring(1,lineArray(0).length-1)
      val direction = lineArray(5).substring(1,lineArray(5).length-1)
      val snodeId = lineArray(9).substring(1,lineArray(9).length-1)
      val enodeId = lineArray(10).substring(1,lineArray(10).length-1)
      val linkId = lineArray(1).substring(1,lineArray(1).length-1)

        mapIdList.+=(Array(mapId,snodeId,direction,linkId))
        mapIdList.+=(Array(mapId,enodeId,direction,linkId))



    }

    val mapIdSortedList = mapIdList.sortWith((x1,x2)=>{(x1(0)<x2(0))||(x1(0)==x2(0)&&x1(1)<x2(1))})

//    mapIdSortedList.foreach(x=>{if(x.size==4) writer.print(x(0)+","+x(1)+","+x(2)+","+x(3)+"\n");else{ println("wrong!")}})
    mapIdSortedList.foreach(x=>{if(x.size==4) writer.print(toStringForArray(x));else{ println("wrong!")}})

    def toStringForArray(x:Array[String]):String={
      var snodeId:StringBuffer = new StringBuffer(x(0))
      for(i <- x(0).length to 16-x(1).length){
        snodeId.append("0")
      }
      snodeId.append(x(1))
      var linkId:StringBuffer = new StringBuffer(x(0))
      for(i <- x(0).length to 13-x(3).length){
        linkId.append("0")
      }
      linkId.append(x(3))
//      var str:StringBuffer = new StringBuffer(x(0)+","+x(1)+","+x(2)+","+x(3))
//      for(i<- str.length to 42){
//        str.append(" ")
//      }
//      str.append("\n")
//      str.toString
      if(snodeId.length()==17 && linkId.length()== 14){
        x(0)+","+snodeId.toString+","+x(2)+","+linkId.toString+"\r\n"
      }
      else{
        println(snodeId.toString+"'s size is wrong!")
        null
      }

    }
    writer.close()



  }

//经纬度信息与路链信息匹配的文件
def generateLinkIdWithLonNLat(): Unit ={
  val mapFile1 =Source.fromFile(inputMid)
  val mapFile2 =Source.fromFile(resultLinkNodeRead)

  val writer = new PrintWriter(new File(resultgenerateLinkIdWithLonNLat ))

  val lines1 = mapFile1.getLines()
  val lines2 = mapFile2.getLines()
  var mapIdList = ListBuffer[Array[String]]()

  while(lines1.hasNext && lines2.hasNext){
    val lineArray  = lines1.next().split(",")
    val lonNlatArray = lines2.next().split(",")
    val mapId = lineArray(0).substring(1,lineArray(0).length-1)
//    val direction = lineArray(5).substring(1,lineArray(5).length-1)
//    val snodeId = lineArray(9).substring(1,lineArray(9).length-1)
//    val enodeId = lineArray(10).substring(1,lineArray(10).length-1)
    val linkIdTemp = lineArray(1).substring(1,lineArray(1).length-1)
    val sNodeLonNlat = lonNlatArray(0)+","+lonNlatArray(1)
    val eNodeLonNlat = lonNlatArray(2)+","+lonNlatArray(3)

    var linkId:StringBuffer = new StringBuffer(mapId)
    for(i <- mapId.length to 13-linkIdTemp.length){
      linkId.append("0")
    }
    linkId.append(linkIdTemp)
    writer.println(linkId+","+sNodeLonNlat+","+eNodeLonNlat)

//    mapIdList.+=(Array(mapId,snodeId,direction,linkId,sNodeLonNlat))
//    mapIdList.+=(Array(mapId,enodeId,direction,linkId,eNodeLonNlat))



  }


  writer.close()



}



  //附带经纬度信息的NextLineResult文件生成
  def generateMapSnodeIdWithLonNLat(): Unit ={
      val mapFile1 =Source.fromFile(inputMid)
      val mapFile2 =Source.fromFile(resultLinkNodeRead)

      val writer = new PrintWriter(new File(resultgenerateMapSnodeIdWithLonNLat ))

      val lines1 = mapFile1.getLines()
      val lines2 = mapFile2.getLines()
      var mapIdList = ListBuffer[Array[String]]()

      while(lines1.hasNext && lines2.hasNext){
        val lineArray  = lines1.next().split(",")
        val lonNlatArray = lines2.next().split(",")
        val mapId = lineArray(0).substring(1,lineArray(0).length-1)
        val direction = lineArray(5).substring(1,lineArray(5).length-1)
        val snodeId = lineArray(9).substring(1,lineArray(9).length-1)
        val enodeId = lineArray(10).substring(1,lineArray(10).length-1)
        val linkId = lineArray(1).substring(1,lineArray(1).length-1)
        val sNodeLonNlat = lonNlatArray(0)+","+lonNlatArray(1)
        val eNodeLonNlat = lonNlatArray(2)+","+lonNlatArray(3)

        mapIdList.+=(Array(mapId,snodeId,direction,linkId,sNodeLonNlat))
        mapIdList.+=(Array(mapId,enodeId,direction,linkId,eNodeLonNlat))



      }

    val mapIdSortedList = mapIdList.sortWith((x1,x2)=>{(x1(0).toLong<x2(0).toLong)||(x1(0).toLong==x2(0).toLong&&x1(1).toLong<x2(1).toLong)})

    //    mapIdSortedList.foreach(x=>{if(x.size==4) writer.print(x(0)+","+x(1)+","+x(2)+","+x(3)+"\n");else{ println("wrong!")}})
    mapIdSortedList.foreach(x=>{if(x.size==5) writer.print(toStringForArray(x));else{ println("wrong!")}})


//    var strSize = 0

    def toStringForArray(x:Array[String]):String={
      var snodeId:StringBuffer = new StringBuffer(x(0))
      for(i <- x(0).length to 16-x(1).length){
        snodeId.append("0")
      }
      snodeId.append(x(1))
      var linkId:StringBuffer = new StringBuffer(x(0))
      for(i <- x(0).length to 13-x(3).length){
        linkId.append("0")
      }
      linkId.append(x(3))

      //      var str:StringBuffer = new StringBuffer(x(0)+","+x(1)+","+x(2)+","+x(3))
      //      for(i<- str.length to 42){
      //        str.append(" ")
      //      }
      //      str.append("\n")
      //      str.toString
      if(snodeId.length()==17 && linkId.length()== 14) {
        val str = x(0) + "," + snodeId.toString + "," + x(2) + "," + linkId.toString + "," + x(4) + "\r\n"
//        if (strSize != str.length) {
//          strSize = str.length
//          println(strSize)
//        }
        if(str.length!=62){println(str.length)}
        str

      }
      else{
        println(snodeId.toString+"'s size is wrong!")
        null
      }

    }
    writer.close()



  }

  //NextLineIndex文件生成,如果是带经纬度的，lineArray的size应该为6，否则为4
  def generateMapSnodeIdIndex(): Unit ={
    val mapFile1 =Source.fromFile(resultgenerateMapSnodeIdWithLonNLat)

    val writer = new PrintWriter(new File(resultgenerateMapSnodeIdIndex ))

    val lines = mapFile1.getLines()
    var currentMapId = ""
//    var pastMapId = ""
    var index = 0

    while(lines.hasNext){
      val lineArray = lines.next().split(",")
      if(lineArray.size == 6){
        val tempMapId = lineArray(0)
//        if(currentMapId == "" ){
//          currentMapId = tempMapId
//        }
//        else
        if (currentMapId!=tempMapId){
          writer.println(tempMapId+","+(index*62))

          currentMapId = tempMapId


        }


      }
      index+=1

    }
    writer.close()
  }

  //写一个函数将nodeId对应的经纬度找出
  def nodeIdMathLonNLat(): Unit ={
    val mapFile = Source.fromFile(resultgenerateMapSnodeIdWithLonNLat)
    val writer = new PrintWriter(new File(resultnodeIdMathLonNLat))
    var nodeMap = SortedMap[Long,String]()


    val lines = mapFile.getLines()
    while(lines.hasNext){
      val lineArray = lines.next().split(",")
      if(lineArray.size==6){
        val nodeId = lineArray(1).toLong
        val lonNlat = lineArray(4)+","+lineArray(5)
        if(nodeMap.contains(nodeId) && !nodeMap(nodeId).equals(lonNlat)){

          println("map::"+nodeMap.get(nodeId)+",  "+"line::"+lonNlat)

        }
        else {
          nodeMap+=(nodeId->lonNlat)
        }
      }

    }
    nodeMap.keys.foreach(id=>{writer.println(id+","+nodeMap(id))})

    writer.close()

  }


  def gpsDataProcess(): Unit ={
    val mapFile1 =Source.fromFile("E:\\coding\\lab\\scala\\data\\BP2968-data-test.csv")

    val writer = new PrintWriter(new File("BP2968-data-processed.csv" ))

    val lines = mapFile1.getLines()
    var lon = ""
    var lat = ""
    while(lines.hasNext){
      val line = lines.next()
      val lineArray = line.split(",")

      if(lineArray.size== 4){
        val tempLon = lineArray(2)
        val tempLat = lineArray(3)
        if((!lon.equals(tempLon))||(!lat.equals(tempLat)))
          {
            writer.println(line)

          }
        lon = tempLon
        lat = tempLat
      }
    }

    writer.close()
  }



}
