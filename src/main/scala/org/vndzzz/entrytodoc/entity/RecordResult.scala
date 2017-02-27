package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.{@@, _}

/**
  * Created by vn on 28.12.2016.
  */
trait RecordId
case class RecordResult(email: String = "",
                        datetime: String,
                        fio: String,
                        doctorData: String,
                        lpuName: String,
                        lpuAddress: String,
                        pol: String = "",
                        stubNum: String,
                        lpuCode: String @@ LpuCode = "".taggedWith[LpuCode],
                        id: String @@ RecordId = "".taggedWith[RecordId],
                        polisAlias: String = "") {
  def toText: String =
    s"<b>Дата и время приёма: </b><code>$datetime</code>\n" +
      s"<b>Номер талона: </b><code>$stubNum</code>\n" +
      s"<b>Врач: </b><code>$fio</code>, <code>$doctorData</code>\n" +
      s"<b>Медицинское учреждение: </b><code>$lpuName</code>\n" +
      s"<b>Адрес: </b><code>$lpuAddress</code>\n" + {
      if (polisAlias.isEmpty) s"<b>Полис:</b> <code>$pol</code>"
      else s"<b>Пациент:</b> <code>$polisAlias</code>"
    }
}
