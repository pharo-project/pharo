tests
testEmptyLcs1

	| patch |
	patch := self patchSequenceFor: #(a b c) and: #().
	self assert: patch size = 3.
	self assert: (patch allSatisfy: [ :each | each key = #remove ])