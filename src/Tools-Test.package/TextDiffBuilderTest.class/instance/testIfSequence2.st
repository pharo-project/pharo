tests
testIfSequence2

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(c d b a).
	self assert: patch size = 6.	"lcs is cd"
	self assert: (patch count: [ :each | each key = #match ]) = 2.
	self assert: (patch count: [ :each | each key = #insert ]) = 2.
	self assert: (patch count: [ :each | each key = #remove ]) = 2.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('cd' includes: each value first) ]
			ifFalse: [ self assert: ('ab' includes: each value first) ] ]