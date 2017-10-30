tests
testContinuationExample1

   | array |
    array := (1 to: 20) asOrderedCollection.
   self assert: ((self continuationExample1: array) = array)
  