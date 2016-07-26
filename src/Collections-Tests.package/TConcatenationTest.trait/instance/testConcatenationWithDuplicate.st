tests - concatenation
testConcatenationWithDuplicate


| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self firstCollection .
result := collection1 , collection2.

result do: [ :each | self assert: (result occurrencesOf: each) = (( collection1 occurrencesOf: each ) + ( collection2 occurrencesOf: each ) ). ].
self assert: result size = (collection1 size * 2)