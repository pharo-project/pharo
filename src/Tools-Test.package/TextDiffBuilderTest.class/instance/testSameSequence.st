tests
testSameSequence

	| patch |
	patch := self patchSequenceFor: #(a b c) and: #(a b c).
	self assert: patch size = 3.	
	self assert: (patch allSatisfy: [ :each | each key = #match ])