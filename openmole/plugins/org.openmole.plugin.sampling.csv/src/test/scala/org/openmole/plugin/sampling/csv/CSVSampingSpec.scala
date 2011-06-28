/*
 * Copyright (C) 2011 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.sampling.csv

import au.com.bytecode.opencsv.CSVReader
import java.io.File
import java.io.FileReader
import org.openmole.misc.exception.UserBadDataError
import org.openmole.core.implementation.data.Prototype
import org.openmole.core.implementation.data.Context
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.openmole.misc.tools.io.FileUtil._

@RunWith(classOf[JUnitRunner])
class CSVSamplingSpec extends FlatSpec with ShouldMatchers {
  
  "CSVSampling" should "detect the correct mapping between csv header defined column" in {
    
    val p1 = new Prototype("col1", classOf[String])
    val p2 = new Prototype("col2", classOf[Int])
    
    val tmpCsvFile = new File("/tmp/tmp.csv")
    getClass.getClassLoader.getResourceAsStream("csvTest.csv").copy(tmpCsvFile)
    val reader = new CSVReader(new FileReader(tmpCsvFile))
    val sampling = new CSVSampling(tmpCsvFile)
    sampling.addColumn(p1)
    
    sampling.build(new Context).head.head.value should equal ("myTest")
    
    sampling.addColumnAs("badName",p2)
    val exception = evaluating { sampling.build(new Context)} should produce [UserBadDataError]
  }
}
