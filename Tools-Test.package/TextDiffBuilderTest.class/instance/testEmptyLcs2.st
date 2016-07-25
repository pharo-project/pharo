tests
testEmptyLcs2

	| patch |
	patch := self patchSequenceFor: #() and: #(a b c).
	self assert: patch size = 3.	
	self assert: (patch allSatisfy: [ :each | each key = #insert ])