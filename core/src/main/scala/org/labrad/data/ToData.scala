package org.labrad.data

import java.util.Date
import org.labrad.types._

trait ToData[T] {
  def pat: Pattern
  def apply(b: DataBuilder, value: T): Unit
  def apply(value: T): Data = {
    val b = DataBuilder()
    apply(b, value)
    b.build()
  }
}

object ToData {
  implicit val dataToData = new ToData[Data] {
    def pat = PAny
    def apply(b: DataBuilder, value: Data): Unit = b.add(value)
    override def apply(value: Data): Data = value
  }
  implicit val boolToData = new ToData[Boolean] {
    def pat = TBool
    def apply(b: DataBuilder, value: Boolean): Unit = b.bool(value)
  }
  implicit val intToData = new ToData[Int] {
    def pat = TInt
    def apply(b: DataBuilder, value: Int): Unit = b.int(value)
  }
  implicit val uintToData = new ToData[Long] {
    def pat = TUInt
    def apply(b: DataBuilder, value: Long): Unit = b.uint(value)
  }
  implicit val stringToData = new ToData[String] {
    def pat = TStr
    def apply(b: DataBuilder, value: String): Unit = b.string(value)
  }
  implicit val dateToData = new ToData[Date] {
    def pat = TTime
    def apply(b: DataBuilder, value: Date): Unit = { val t = TimeStamp(value); b.time(t.seconds, t.fraction) }
  }
  implicit def valueToData = new ToData[Double] {
    def pat = TValue(None)
    def apply(b: DataBuilder, value: Double): Unit = b.value(value)
  }
  def valueToData(unit: String) = new ToData[Double] {
    def pat = TValue(Some(unit))
    def apply(b: DataBuilder, value: Double): Unit = b.value(value, unit)
  }
  implicit def complexToData = new ToData[Complex] {
    def pat = TComplex(None)
    def apply(b: DataBuilder, value: Complex): Unit = b.complex(value.real, value.imag)
  }
  def complexToData(unit: String) = new ToData[Complex] {
    def pat = TComplex(Some(unit))
    def apply(b: DataBuilder, value: Complex): Unit = b.complex(value.real, value.imag, unit)
  }

  implicit def tuple2ToData[T1, T2](implicit s1: ToData[T1], s2: ToData[T2]) = new ToData[(T1, T2)] {
    def pat = PCluster(s1.pat, s2.pat)
    def apply(b: DataBuilder, value: (T1, T2)): Unit = {
      value match {
        case (v1, v2) => b.cluster(2); s1(b, v1); s2(b, v2)
      }
    }
  }

  implicit def tuple3ToData[T1, T2, T3](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3]) = new ToData[(T1, T2, T3)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3)): Unit = {
      value match {
        case (v1, v2, v3) => b.cluster(3); s1(b, v1); s2(b, v2); s3(b, v3)
      }
    }
  }

  implicit def tuple4ToData[T1, T2, T3, T4](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3], s4: ToData[T4]) = new ToData[(T1, T2, T3, T4)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat, s4.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3, T4)): Unit = {
      value match {
        case (v1, v2, v3, v4) => b.cluster(4); s1(b, v1); s2(b, v2); s3(b, v3); s4(b, v4)
      }
    }
  }

  implicit def tuple5ToData[T1, T2, T3, T4, T5](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3], s4: ToData[T4], s5: ToData[T5]) = new ToData[(T1, T2, T3, T4, T5)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat, s4.pat, s5.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3, T4, T5)): Unit = {
      value match {
        case (v1, v2, v3, v4, v5) => b.cluster(5); s1(b, v1); s2(b, v2); s3(b, v3); s4(b, v4); s5(b, v5)
      }
    }
  }

  implicit def tuple6ToData[T1, T2, T3, T4, T5, T6](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3], s4: ToData[T4], s5: ToData[T5], s6: ToData[T6]) = new ToData[(T1, T2, T3, T4, T5, T6)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat, s4.pat, s5.pat, s6.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3, T4, T5, T6)): Unit = {
      value match {
        case (v1, v2, v3, v4, v5, v6) => b.cluster(6); s1(b, v1); s2(b, v2); s3(b, v3); s4(b, v4); s5(b, v5); s6(b, v6)
      }
    }
  }

  implicit def tuple7ToData[T1, T2, T3, T4, T5, T6, T7](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3], s4: ToData[T4], s5: ToData[T5], s6: ToData[T6], s7: ToData[T7]) = new ToData[(T1, T2, T3, T4, T5, T6, T7)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat, s4.pat, s5.pat, s6.pat, s7.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3, T4, T5, T6, T7)): Unit = {
      value match {
        case (v1, v2, v3, v4, v5, v6, v7) => b.cluster(7); s1(b, v1); s2(b, v2); s3(b, v3); s4(b, v4); s5(b, v5); s6(b, v6); s7(b, v7)
      }
    }
  }

  implicit def tuple8ToData[T1, T2, T3, T4, T5, T6, T7, T8](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3], s4: ToData[T4], s5: ToData[T5], s6: ToData[T6], s7: ToData[T7], s8: ToData[T8]) = new ToData[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat, s4.pat, s5.pat, s6.pat, s7.pat, s8.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3, T4, T5, T6, T7, T8)): Unit = {
      value match {
        case (v1, v2, v3, v4, v5, v6, v7, v8) => b.cluster(8); s1(b, v1); s2(b, v2); s3(b, v3); s4(b, v4); s5(b, v5); s6(b, v6); s7(b, v7); s8(b, v8)
      }
    }
  }

  implicit def tuple9ToData[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit s1: ToData[T1], s2: ToData[T2], s3: ToData[T3], s4: ToData[T4], s5: ToData[T5], s6: ToData[T6], s7: ToData[T7], s8: ToData[T8], s9: ToData[T9]) = new ToData[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    def pat = PCluster(s1.pat, s2.pat, s3.pat, s4.pat, s5.pat, s6.pat, s7.pat, s8.pat, s9.pat)
    def apply(b: DataBuilder, value: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Unit = {
      value match {
        case (v1, v2, v3, v4, v5, v6, v7, v8, v9) => b.cluster(9); s1(b, v1); s2(b, v2); s3(b, v3); s4(b, v4); s5(b, v5); s6(b, v6); s7(b, v7); s8(b, v8); s9(b, v9)
      }
    }
  }
}
