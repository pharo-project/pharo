tests
testGpsExample2

  | result array |

  "  integer matrix elements should be used for the purpose of this test. " 
  array := #(#(1 2 3 4 5) #(6 7 8 9 10) #(11 12 13 14 15) #(16 17 18 19 20) #(21 22 23 24 25)).
  result := array inject: 0
              into: [:sum :subarray |
                      sum + (subarray inject: 0
                                          into: [:s :elem | s + elem])].
 self assert: ((self gpsExample2: array) = result)
  