/*
Copyright 2023 Chris Basinger

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

import scala.util.Sorting

extension(list: Iterable[Double])
  def toCentralTendencyMeasurements(): CentralTendencyMeasurements = {
    val sortedList = Sorting.stableSort(list.toSeq).toList
    var counts = Map[Double, Double]()
    val map = sortedList.groupBy(identity).mapValues(_.size)
    val mode = map.maxBy(pair => pair._2)._1
    var median = 0.0
    if sortedList.size % 2 == 0 then
      median = (sortedList(sortedList.size / 2 - 1) + sortedList(sortedList.size / 2)) / 2.0
    else
      median = sortedList(sortedList.size / 2)
    val mean = sortedList.sum.toDouble / sortedList.size
    CentralTendencyMeasurements(mode, median, mean)
  }
