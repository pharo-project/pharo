jensen device examples
gpsExample2: aCollection

   " BlockClosuresTestCase new 
       gpsExample2: #(#(1 2 3 4 5) #(6 7 8 9 10) #(11 12 13 14 15) #(16 17 18 19 20) #(21 22 23 24 25))"

    | js i j |

  "  js is the translation of the Algol procedure from method comment. "
  js := [:a :idx :n | | sum |
               sum := 0.
               idx first value: 1.
               [idx last value <= n last value]
                 whileTrue:
                   [sum :=  sum  + a last value. 
                    idx first value:  idx last value + 1.].
              sum
             ].

  "  This is the most complicated call that is mentioned in method comment. Note that  js  is called recursively. "

  ^ js value: (Array with: [:val | self error: 'can not assign to procedure']
                        with: [ js value: (Array with: [:val | (aCollection at: i) at: j put: val]
                                                     with: [ (aCollection at: i) at: j])
                                   value: (Array with:[:val | j := val]
                                                     with: [ j])
                                   value: (Array with: [:val | self error: 'can not assign to constant']
                                                     with: [ aCollection size])
                               ]
               )
    value: (Array with:[:val | i := val]
                  with: [ i])
    value: (Array with: [:val | self error: 'can not assign to constant']
                  with: [ aCollection size])

