package org.hinz.freida

import com.mongodb.casbah.Imports._

class FreidaDAO {
  val mongoConn = MongoConnection()
  val mongoDB = mongoConn("freida")

  val programs = mongoDB("programs")

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
}
