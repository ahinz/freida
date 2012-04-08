package org.hinz.freida

import com.mongodb.casbah.Imports._

object mongo extends MongoConversions {

  class FreidaDAO {
    val mongoConn = MongoConnection()
    val mongoDB = mongoConn("freida")
    
    val programs = mongoDB("programs")

    def programIds() = programs find() map (a => a.get("pid").toString) toList
    
    def findProgram(pid: String):Option[Program] =
      programs.findOne(MongoDBObject("pid" -> pid)) map (_.get("info")) flatMap fromMongo[Program]

    def insertProgram(pid: String, state: String, spec: String):Boolean = {
      val program = MongoDBObject("pid" -> pid, "state" -> state, "spec" -> spec)
      
      programs.findOne(program) match {
        case Some(_) => false
        case None => {
        programs += program
          true
        }
      }
    }

    def updateProgram(pid: String, payload: AnyRef):Boolean = {
      val program = MongoDBObject("pid" -> pid)

      programs.findOne(program) match {
        case Some(p) => {p += ("info" -> payload); programs += p; true }
        case None => false
      }
    }
  }
}


trait MongoConversions {

  trait MongoStorable[T] {
    def toMongo(t: T):AnyRef
    def fromMongo(a: Any):Option[T]
    def fromMongoUnsafe(a: Any) = fromMongo(a) get

    def safeConvertMDL(a: Any)(implicit ev:com.mongodb.BasicDBList => MongoDBList):Option[MongoDBList] =
      safeConvert[com.mongodb.BasicDBList](a) map ev

    def safeConvertMDB(a: Any)(implicit ev:com.mongodb.BasicDBObject => MongoDBObject):Option[MongoDBObject] =
      safeConvert[com.mongodb.BasicDBObject](a) map ev

    def safeConvert[T:Manifest](a: Any):Option[T] = {
      val erasure = manifest[T] match {
        case Manifest.Byte => classOf[java.lang.Byte]
        case Manifest.Short => classOf[java.lang.Short]
        case Manifest.Char => classOf[java.lang.Character]
        case Manifest.Long => classOf[java.lang.Long]
        case Manifest.Float => classOf[java.lang.Float]
        case Manifest.Double => classOf[java.lang.Double]
        case Manifest.Boolean => classOf[java.lang.Boolean]
        case Manifest.Int => classOf[java.lang.Integer]
        case m => m.erasure
      }

      if(erasure.isInstance(a)) Some(a.asInstanceOf[T]) else {
        println("Failed to convert " + a + " to " + erasure + " orginally of type " + a.getClass)
        None
      }
    }

    def checkClass(m: MongoDBObject, clazz: String):Option[MongoDBObject] =
      m.get("__class") filter (_ toString() equals(clazz)) map (a => m)
  }

  def asMongo[T](t: T)(implicit m: MongoStorable[T]):AnyRef = m.toMongo(t)
  def fromMongo[T](a: Any)(implicit m: MongoStorable[T]):Option[T] = m.fromMongo(a)

  implicit def stringIsMongoObj:MongoStorable[String] = new MongoStorable[String] {
    def toMongo(s: String) = s
    def fromMongo(a: Any) = safeConvert[String](a)
  }

  implicit def listIsMongoObj[T:MongoStorable]:MongoStorable[List[T]] = new MongoStorable[List[T]] {
    def toMongo(l: List[T]) = {
      val valTx = implicitly[MongoStorable[T]]
      MongoDBObject("__class" -> "list",
                    "list" -> l.map(valTx.toMongo(_)))
    }
    def fromMongo(a: Any) = 
      safeConvertMDB(a) flatMap (f =>
        checkClass(f, "list")) flatMap 
          (_.get("list")) flatMap (f =>
            safeConvertMDL(f)) map (list =>
              list map (x => implicitly[MongoStorable[T]].fromMongo(x).get) toList)
  }

  implicit def mapIsMongoObj[T:MongoStorable]:MongoStorable[Map[String,T]] = new MongoStorable[Map[String,T]] {
    def toMongo(m: Map[String,T]) = {
      val builder = MongoDBObject.newBuilder

      val valTx = implicitly[MongoStorable[T]]

      m map {
        case (k,v) => builder += (k.replace(".","__dot__") -> valTx.toMongo(v))
      }

      builder.result
    }

    def flipOpt[T](s: Iterable[Option[T]]):Option[Seq[T]] = s.foldLeft(Some(List()):Option[List[T]])((s,x) => s.flatMap(a => x.map(b => b :: a))) map (_.reverse)

    def fromMongo(a: Any) = 
      safeConvertMDB(a) map (themap =>
        themap.keys map (key => {
          val newkey = key.replace("__dot__",".")
          themap.get(key).flatMap(a =>  implicitly[MongoStorable[T]].fromMongo(a)) match {
            case Some(a) => Some(newkey -> a)
            case None => None
          }
        })) flatMap (a => flipOpt(a)) map (a => Map(a:_*))
  }

  implicit def tuple2IsMongoObj[T1:MongoStorable, T2:MongoStorable]:MongoStorable[(T1,T2)] = new MongoStorable[(T1,T2)] {
    def toMongo(t: (T1,T2)) = MongoDBObject("__class" -> "__tuple2",
                                               "t1" -> implicitly[MongoStorable[T1]].toMongo(t._1),
                                               "t2" -> implicitly[MongoStorable[T2]].toMongo(t._2))

    def fromMongo(a: Any) = 
      safeConvertMDB(a) flatMap (m =>
        checkClass(m, "__tuple2")) flatMap (m =>
          for (t1 <- m.get("t1").flatMap(a => implicitly[MongoStorable[T1]].fromMongo(a)) ;
               t2 <- m.get("t2").flatMap(a => implicitly[MongoStorable[T2]].fromMongo(a))) yield (t1,t2))
  }

  implicit def tuple3IsMongoObj[T1:MongoStorable, T2:MongoStorable, T3:MongoStorable]:MongoStorable[(T1,T2,T3)] = new MongoStorable[(T1,T2,T3)] {
    def toMongo(t: (T1,T2,T3)) = MongoDBObject("__class" -> "__tuple3",
                                               "t1" -> implicitly[MongoStorable[T1]].toMongo(t._1),
                                               "t2" -> implicitly[MongoStorable[T2]].toMongo(t._2),
                                               "t3" -> implicitly[MongoStorable[T3]].toMongo(t._3))

    def fromMongo(a: Any) = 
      safeConvertMDB(a) flatMap (m =>
        checkClass(m, "__tuple3")) flatMap (m =>
          for (t1 <- m.get("t1").flatMap(a => implicitly[MongoStorable[T1]].fromMongo(a)) ;
               t2 <- m.get("t2").flatMap(a => implicitly[MongoStorable[T2]].fromMongo(a)) ;
               t3 <- m.get("t3").flatMap(a => implicitly[MongoStorable[T3]].fromMongo(a))) yield (t1,t2, t3))
  }

  implicit def optionIsMongoObj[T:MongoStorable]:MongoStorable[Option[T]] = new MongoStorable[Option[T]] {
    def toMongo(o: Option[T]) = o match {
      case Some(t) => MongoDBObject("__class" -> "__option",
                                    "value" -> implicitly[MongoStorable[T]].toMongo(t))
      case None => MongoDBObject(("__class" -> "__option"))
    }

    def fromMongo(a: Any) = 
      safeConvertMDB(a) flatMap (m => 
        checkClass(m, "__option")) flatMap (m =>
          m.get("value") match {
            case Some(q) => implicitly[MongoStorable[T]].fromMongo(q).map(a => Some(a))
            case None => Some(None)
          })
  }
      
  implicit def contactIsMongoObj: MongoStorable[Contact] = new MongoStorable[Contact] {
    def toMongo(f: Contact) = {
      MongoDBObject("__class" -> "Contact",
                    "name" -> f.name,
                    "address" -> f.address,
                    "contact" -> asMongo(f.contact))
    }

    def fromMongo(a: Any) = 
      for(map <- safeConvertMDB(a) ;
          cmap <- checkClass(map, "Contact") ;
          name <- cmap.get("name") map (_.toString) ;
          address <- cmap.get("address") map (_.toString);
          contactraw <- cmap.get("contact") ;
          contact <- implicitly[MongoStorable[Map[String,String]]].fromMongo(contactraw)) yield
            Contact(name, address, contact)
          
          
  }

  implicit def facultyIsMongoObj: MongoStorable[Faculty] = new MongoStorable[Faculty] {
    def toMongo(f: Faculty) = {
      MongoDBObject("__class" -> "Faculty",
                    "fullTimePhy" -> f.fullTimePhy,
                    "fullTimeNonPhy" -> f.fullTimeNonPhy,
                    "partTimePhy" -> f.partTimePhy,
                    "partTimeNonPhy" -> f.partTimeNonPhy)
    }

    def fromMongo(a: Any) = 
      for(map <- safeConvertMDB(a) ;
          cmap <- checkClass(map, "Faculty") ;
          ftp  <- cmap get("fullTimePhy") map (_.toString.toInt) ;
          ftnp <- cmap get("fullTimeNonPhy") map (_.toString.toInt) ;
          ptp  <- cmap get("partTimePhy") map (_.toString.toInt) ;
          ptnp <- cmap get("partTimeNonPhy") map (_.toString.toInt)) yield
            Faculty(ftp,ftnp,ptp,ptnp)

  }

  implicit def programIsMongoObj: MongoStorable[Program] = new MongoStorable[Program] {
    def toMongo(p: Program) = {
      MongoDBObject(("__class" -> "Program"),
                    ("pId" -> asMongo(p.programId)),
                    ("lastUpdated" -> asMongo(p.lastUpdated)),
                    ("surveyReceived" -> asMongo(p.surveyReceived)),
                    ("director" -> asMongo(p.director)),
                    ("contact" -> asMongo(p.contact)),
                    ("webAddr" -> asMongo(p.webAddr)),
                    ("basicInfo" -> asMongo(p.basicInfo)),
                    ("institutions" -> asMongo(p.institutions)),
                    ("programSize" -> asMongo(p.programSize)),
                    ("programInfo" -> asMongo(p.programInfo)),
                    ("usmleReqs" -> asMongo(p.usmleReqs)),
                    ("faculty" -> asMongo(p.faculty)),
                    ("hours" -> asMongo(p.hours)),
                    ("callSchedule" -> asMongo(p.callSchedule)),
                    ("education" -> asMongo(p.education)),
                    ("evals" -> asMongo(p.evals)),
                    ("benefits" -> asMongo(p.benefits)),
                    ("salary" -> asMongo(p.salary)))
    }

    // Note that outer option represents a load failure
    // and the inner option represents the decoded option
    def optExt[T:MongoStorable](a: Any):Option[Option[T]] = implicitly[MongoStorable[Option[T]]].fromMongo(a)

    def fromMongo(a: Any) = 
      for(map <- safeConvertMDB(a) ;
          cmap <- checkClass(map, "Program") ;
          pId <- cmap.get("pId") map (_.toString) ;
          lastUpdated <- cmap.get("lastUpdated") flatMap optExt[String] ;
          surveyReceived <- cmap.get("surveyReceived") flatMap optExt[String] ;
          director <- cmap.get("director") flatMap (f => implicitly[MongoStorable[Option[Contact]]].fromMongo(f)) ;
          contact <- cmap.get("contact") flatMap (f => implicitly[MongoStorable[Option[Contact]]].fromMongo(f)) ;
          webAddr <- cmap.get("webAddr") map (_.toString) ;
          basicInfo <- cmap.get("basicInfo") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          institutions <- cmap.get("institutions") flatMap (f => implicitly[MongoStorable[Option[Map[String,List[String]]]]].fromMongo(f)) ;
          programSize <- cmap.get("programSize") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          programInfo <- cmap.get("programInfo") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          usmleReqs <- cmap.get("usmleReqs") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          faculty <- cmap.get("faculty") flatMap optExt[Faculty] ;
          hours <- cmap.get("hours") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          callSchedule <- cmap.get("callSchedule") flatMap (f => implicitly[MongoStorable[Option[Map[String,(String,String)]]]].fromMongo(f)) ;
          education <- cmap.get("education") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          evals <- cmap.get("evals") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          benefits <- cmap.get("benefits") flatMap (f => implicitly[MongoStorable[Option[Map[String,String]]]].fromMongo(f)) ;
          salary <- cmap.get("salary") flatMap (f => implicitly[MongoStorable[Option[Map[String,(String,String,String)]]]].fromMongo(f))) yield
            Program(pId, lastUpdated, surveyReceived, director, contact, webAddr, basicInfo, institutions, programSize, 
                    programInfo, usmleReqs, faculty, hours, callSchedule, education, evals, benefits, salary)

  }

}


