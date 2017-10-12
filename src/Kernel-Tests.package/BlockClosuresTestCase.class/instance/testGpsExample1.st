tests
testGpsExample1

  | result array |

  array := (1 to: 100) asArray.
  result := array inject: 0
              into: [:sum :val | sum + val].
 self assert: ((self gpsExample1: array) = result)
  