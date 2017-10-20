tests
testContinuationExample3

   | array |
    array := (1 to: 20) asOrderedCollection.
   self assert: ((self continuationExample3: array) = (array collect: [:x | x * x - 10]))
  