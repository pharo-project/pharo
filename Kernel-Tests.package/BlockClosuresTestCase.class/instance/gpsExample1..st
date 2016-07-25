jensen device examples
gpsExample1: aCollection

   " BlockClosuresTestCase new gpsExample1: (1 to: 100) asArray"

 | gps i  s |

  gps := [:idx :exp :sum | | cnt |
               cnt := 1.
               sum first value: 0.
               [idx first value: cnt.
                sum first value: (sum last value + exp last value). 
                cnt := cnt + 1.
                cnt <= aCollection size] whileTrue: [   ].
              sum last value
             ].


  ^gps value: (Array with: [:val | i := val]
                    with: [ i])
      value: (Array with: [:val | aCollection at: i put:  val]
                    with: [ aCollection at: i])
      value: (Array with: [:val | s := val]
                    with: [ s])
