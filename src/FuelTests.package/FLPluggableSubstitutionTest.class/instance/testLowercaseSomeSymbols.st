tests
testLowercaseSomeSymbols

	| result |
	self analyzer when: [:x | #A = x] substituteBy: [:x | #a].
	self analyzer when: [:x | #B = x] substituteBy: [:x | #b].

	result := self resultOfSerializeAndMaterialize: #(A B C).
	
	self assert: #(a b C) = result.