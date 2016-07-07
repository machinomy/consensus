package com.machinomy.consensus

import com.machinomy.consensus.state.{GCounter, PNCounter}
import com.machinomy.xicity.Identifier
import scodec._
import scodec.bits.{ByteVector, _}
import scodec.codecs._
import com.machinomy.xicity.transport.Message

case class Row(timestamp: Long, mapping: PNCounter[Identifier, Production])

object Row {
  implicit val timestampCodec = int64L

  implicit val identifierCodec = Message.identifierCodec
  implicit val doubleCodec = doubleL
  implicit val longCodec = vlongL

  implicit val productionCodec = new Codec[Production] {
    override def encode(value: Production): Attempt[BitVector] =
      for {
        productionBytes <- doubleCodec.encode(value.volume)
        costBytes <- doubleCodec.encode(value.cost)
      } yield productionBytes ++ costBytes

    override def sizeBound: SizeBound = doubleCodec.sizeBound + doubleCodec.sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[Production]] =
      for {
        volumeR <- doubleCodec.decode(bits)
        volume = volumeR.value
        costR <- doubleCodec.decode(volumeR.remainder)
        cost = costR.value
      } yield DecodeResult(Production(volume, cost), costR.remainder)
  }

  implicit val mapEntryCodec = new Codec[(Identifier, Production)] {
    override def encode(value: (Identifier, Production)): Attempt[BitVector] =
      for {
        identifierBytes <- identifierCodec.encode(value._1)
        productionBytes <- productionCodec.encode(value._2)
      } yield identifierBytes ++ productionBytes

    override def sizeBound: SizeBound = identifierCodec.sizeBound + productionCodec.sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[(Identifier, Production)]] =
      for {
        identifierR <- identifierCodec.decode(bits)
        identifier = identifierR.value
        doubleR <- productionCodec.decode(identifierR.remainder)
        double = doubleR.value
      } yield DecodeResult((identifier, double), doubleR.remainder)
  }

  implicit val mapCodec = new Codec[Map[Identifier, Production]] {
    val listEntriesCodec = listOfN(int32L, mapEntryCodec)
    override def encode(value: Map[Identifier, Production]): Attempt[BitVector] = listEntriesCodec.encode(value.toList)
    override def sizeBound: SizeBound = listEntriesCodec.sizeBound
    override def decode(bits: BitVector): Attempt[DecodeResult[Map[Identifier, Production]]] =
      for {
        listEntriesR <- listEntriesCodec.decode(bits)
        listEntries = listEntriesR.value
      } yield DecodeResult(listEntries.toMap, listEntriesR.remainder)
  }

  implicit val gCounterCodec = new Codec[GCounter[Identifier, Production]] {
    override def encode(value: GCounter[Identifier, Production]): Attempt[BitVector] = mapCodec.encode(value.state)
    override def sizeBound: SizeBound = mapCodec.sizeBound
    override def decode(bits: BitVector): Attempt[DecodeResult[GCounter[Identifier, Production]]] =
      mapCodec.decode(bits).map { decodeResult =>
        decodeResult.map(state => new GCounter[Identifier, Production](state))
      }
  }

  implicit val pNCounterCodec = new Codec[PNCounter[Identifier, Production]] {
    override def encode(value: PNCounter[Identifier, Production]): Attempt[BitVector] =
      for {
        incrementsBytes <- gCounterCodec.encode(value.increments)
        decrementsBytes <- gCounterCodec.encode(value.decrements)
      } yield incrementsBytes ++ decrementsBytes

    override def sizeBound: SizeBound = gCounterCodec.sizeBound + gCounterCodec.sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[PNCounter[Identifier, Production]]] =
      for {
        incrementsR <- gCounterCodec.decode(bits)
        increments = incrementsR.value
        decrementsR <- gCounterCodec.decode(incrementsR.remainder)
        decrements = decrementsR.value
      } yield DecodeResult(new PNCounter[Identifier, Production](increments, decrements), decrementsR.remainder)
  }

  implicit val rowCodec = new Codec[Row] {
    override def encode(value: Row): Attempt[BitVector] =
      for {
        timestampBytes <- longCodec.encode(value.timestamp)
        mappingBytes <- pNCounterCodec.encode(value.mapping)
      } yield timestampBytes ++ mappingBytes

    override def sizeBound: SizeBound = longCodec.sizeBound + pNCounterCodec.sizeBound
    override def decode(bits: BitVector): Attempt[DecodeResult[Row]] =
      for {
        timestampR <- longCodec.decode(bits)
        timestamp = timestampR.value
        mappingR <- pNCounterCodec.decode(timestampR.remainder)
        mapping = mappingR.value
      } yield DecodeResult(Row(timestamp, mapping), mappingR.remainder)
  }
}
