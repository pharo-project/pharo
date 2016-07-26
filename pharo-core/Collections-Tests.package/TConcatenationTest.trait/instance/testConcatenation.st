tests - concatenation
testConcatenation

"| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self secondCollection .
result := collection1 , collection2.

collection1 do:[ :each | self assert: (result includes: each)].
collection2 do:[ :each | self assert: (result includes: each)]."

| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self secondCollection .
result := collection1 , collection2.

result do: [ :each | self assert: (result occurrencesOf: each) = (( collection1 occurrencesOf: each ) + ( collection2 occurrencesOf: each ) ). ].
self assert: result size = (collection1 size + collection2 size)