tests
testEmptyLcs3

	| patch |
	patch := self patchSequenceFor: #(a b c) and: #(d e f g).
	self assert: patch size = 7.	
	patch do: [ :each |
		each key = #remove ifTrue: [ self assert: ('abc' includes: each value first) ].
		each key = #insert ifTrue: [ self assert: ('defg' includes: each value first) ] ]