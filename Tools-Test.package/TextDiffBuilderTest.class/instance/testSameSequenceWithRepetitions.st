tests
testSameSequenceWithRepetitions

	| patch |
	patch := self patchSequenceFor: #(a a b a) and: #(a a b a).
	self assert: patch size = 4.	
	self assert: (patch allSatisfy: [ :each | each key = #match ])