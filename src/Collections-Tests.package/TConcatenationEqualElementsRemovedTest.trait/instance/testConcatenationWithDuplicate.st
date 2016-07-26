tests - concatenation
testConcatenationWithDuplicate


| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self firstCollection  .
result := collection1 , collection2.

collection1 do:[ :each | self assert: (result includes: each)].
self assert: result size = collection1 size.