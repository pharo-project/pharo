tests
testContinuationExample2

   | array |
    array := (1 to: 20) asOrderedCollection.
   self assert: ((self continuationExample2: array) = (array collect: [:x | x * x]))
  