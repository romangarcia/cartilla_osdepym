package osdepym

import java.time.DayOfWeek
import java.time.format.TextStyle
import java.util.Locale

object OsdepymCrawler extends App {

  import sttp.client3._
  import sttp.client3.upicklejson._
  import upickle.default._

  val backend = HttpClientSyncBackend()

  case class Zone(idZona: String, descripcion: String)

  case class City(idBarrioLocalidad: String, descripcion: String)

  case class Specialty(idEspecialidad: String, descripcion: String)

  case class CareGiver(id: Long, idPrestador: Long, idDomicilio: Long, prestadorCuitCuil: String, prestadorDenominacion: String, prestadorDomicilioCalle: String, prestadorDomicilioNumero: String, prestadorDomicilioTelefonoNumero: String, prestadorDomicilioEspecialidadDescripcion: String, prestadorDomicilioPiso: String, prestadorDomicilioZonaDescripcion: String, horariosPrestadorJson: String)

  case class CareGiverResult(content: List[CareGiver], last: Boolean)

  implicit val zoneRW: ReadWriter[Zone] = macroRW[Zone]
  implicit val cityRW: ReadWriter[City] = macroRW[City]
  implicit val specialtyRW: ReadWriter[Specialty] = macroRW[Specialty]
  implicit val caregiverRW: ReadWriter[CareGiver] = macroRW[CareGiver]
  implicit val caregiverResultRW: ReadWriter[CareGiverResult] = macroRW[CareGiverResult]

  final val INTEREST_PLAN = "P2000"
  final val INTEREST_ALL_CITIES = "0"
  final val INTEREST_ALL_SPECIALTIES = "0"

  final val INTEREST_ZONES = List(
    "1", // CF
    "3", // NORTE
    "7", // OESTE
  )

  final val INTEREST_SPECIALTY = "012"

  def decodeOpenHours(jsonString: String): List[String] = {
    def toDayDisplay(dayNum: Int): String = {
      // 1 -> 7
      // 2 -> 1
      // 3 -> 2
      // ...
      val fixedDayNum = if (dayNum == 1) 7 else dayNum - 1
      DayOfWeek.of(fixedDayNum).getDisplayName(TextStyle.FULL, Locale.forLanguageTag("es")).capitalize
    }
    ujson.read(jsonString).arr.map { js =>
      val sinceDayNum = js("diaDesde").num.toInt
      val toDayNum = js("diaHasta").num.toInt
      val sinceDay = toDayDisplay(sinceDayNum)
      val days = if (sinceDayNum == toDayNum) sinceDay else {
        val toDay = toDayDisplay(toDayNum)
        s"$sinceDay a $toDay"
      }
      val fromHour = js("horaDesde").str
      val toHour = js("horaHasta").str
      s"$days de $fromHour a $toHour"
    }.toList
  }

  def readZones(planId: String = "P2000"): List[Zone] = {
    basicRequest.get(uri"https://www.osdepym.com.ar/PortalCMS/getZonasByPlan.htm?idPlan=$planId").response(asJson[List[Zone]]).send(backend).body match {
      case Left(e) => throw new IllegalStateException("Failed to read zones", e)
      case Right(value) => value
    }
  }

  def readCities(zoneId: String): List[City] = {
    basicRequest.get(uri"https://www.osdepym.com.ar/PortalCMS/getLocalidadesByZona.htm?idZona=$zoneId").response(asJson[List[City]]).send(backend).body match {
      case Left(e) => throw new IllegalStateException("Failed to read zones", e)
      case Right(value) =>
        println(s"Resultado exitoso CIUDADES: $value")
        value
    }
  }

  def readSpecialties(planId: String, zoneId: String): List[Specialty] = {
    println(s"Consultando especialidades para [plan: $planId - zona: $zoneId]")
    basicRequest.cookie("JSESSIONID", "B4A5B3D8DEF4DEA85BB1E51B10BE86E1").acceptEncoding("gzip, deflate, br").header("Accept", "*/*").header("Referer", "https://www.osdepym.com.ar/PortalCMS/app.htm?page=servicios").
      get(uri"https://www.osdepym.com.ar/PortalCMS/getEspecialidadesByZona.htm?idZona=$zoneId&idPlan=$planId").response(asJson[List[Specialty]]).send(backend).body match {
      case Left(e) => throw new IllegalStateException("Failed to read zones", e)
      case Right(value) =>
        value
    }
  }

  def readCaregivers(zoneId: String, planId: String, cityId: String, specialtyId: String, page: Int = 0): List[CareGiver] = {
    println(s"Consultando prestadores para [plan: $planId - zona: $zoneId, ciudad: $cityId, especialidad: $specialtyId]")
    basicRequest.cookie("JSESSIONID", "B4A5B3D8DEF4DEA85BB1E51B10BE86E1").acceptEncoding("gzip, deflate, br").header("Accept", "*/*").header("Referer", "https://www.osdepym.com.ar/PortalCMS/app.htm?page=servicios").
      get(uri"https://www.osdepym.com.ar/PortalCMS/getPrestadores.htm?idZona=$zoneId&idPlan=$planId&idBarrioLocalidad=$cityId&idEspecialidad=$specialtyId&page=$page").response(asJson[CareGiverResult]).send(backend).body match {
      case Left(e) => throw new IllegalStateException("Failed to read zones", e)
      case Right(value) =>
        if (value.last) value.content
        else {
          // println(s"Reading NEXT PAGE: ${page + 1} for [especialidad: $specialtyId, zona: $zoneId]")
          value.content ++ readCaregivers(zoneId, planId, cityId, specialtyId, page + 1)
        }
    }
  }

  def readPhones(idDirPrestador: String): List[String] = {
    basicRequest.get(uri"https://www.osdepym.com.ar/PortalCMS/getTelefonos.htm?idPrestadorDireccion=$idDirPrestador").response(asJson[List[String]]).send(backend).body match {
      case Left(e) => throw new IllegalStateException("Failed to read phones", e)
      case Right(value) => value
    }
  }

  val results = for {
    zoneId <- INTEREST_ZONES
    caregiver <- readCaregivers(zoneId, INTEREST_PLAN, INTEREST_ALL_CITIES, INTEREST_ALL_SPECIALTIES)
    phones = if (caregiver.prestadorDomicilioTelefonoNumero.isBlank) {
      readPhones(caregiver.idPrestador.toString)
    } else List(caregiver.prestadorDomicilioTelefonoNumero.replaceAll("[\\n\\r\\t]", ""))
  } yield (caregiver, phones)

  println("MEDICO, ESPECIALIDAD, DOMICILIO, LOCALIDAD, TELEFONO, HORARIOS")
  results.foreach { case (c, phones) =>
    val medico = c.prestadorDenominacion.replaceAll(",", " ")
    val domicilio = s"${c.prestadorDomicilioCalle} ${c.prestadorDomicilioNumero} ${c.prestadorDomicilioPiso}"
    val telefono = phones.mkString(" / ")
    val ciudad = c.prestadorDomicilioZonaDescripcion
    val especialidad = c.prestadorDomicilioEspecialidadDescripcion
    val hours = if (c.horariosPrestadorJson != null) decodeOpenHours(c.horariosPrestadorJson).mkString(" / ") else ""
    println(s"$medico, $especialidad, $domicilio, $ciudad, $telefono, $hours")
  }



}
