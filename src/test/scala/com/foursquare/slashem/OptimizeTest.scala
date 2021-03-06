package com.foursquare.slashem

import org.junit.Assert
import org.junit.Test
import org.specs2.mutable.Specification

class OptimizeTest extends  Specification {

  @Test
  def testProduceCorrectListfieldFilterAny {
    val q = SVenueTest where (_.metall any) filter (_.metall any)
    val optimizedQ = q.optimize()
    val qp = q.meta.queryParams(optimizedQ).toList
    Assert.assertEquals(qp.sortWith(_._1 > _._1),
                        List("q" -> "*:*",
                             "start" -> "0",
                             "rows" -> "10").sortWith(_._1 > _._1))
  }

}
