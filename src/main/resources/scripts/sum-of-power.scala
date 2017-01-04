#!/bin/bash

exec /opt/scala-2.10/bin/scala -classpath $(dirname $0)/../../../libs/sum-of-power.jar -feature "$0" "$(basename $0)" "$@"
!#

import com.github.noel.yap.Σp

if (args.length == 2) {
  val σp = Σp(args(1).toInt)

  println(σp.toString)
} else if (args.length == 3) {
  val σp = Σp(args(1).toInt)

  println(σp(args(2).toInt))
} else {
  Console.err.println(s"Usage: ${args(0)} p [n]")
}
