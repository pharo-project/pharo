tests
testIfSequence6

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(d a b c).
	self assert: patch size = 5.	"lcs is abc"
	self assert: (patch count: [ :each | each key = #match ]) = 3.
	self assert: (patch count: [ :each | each key = #insert ]) = 1.
	self assert: (patch count: [ :each | each key = #remove ]) = 1.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('abc' includes: each value first) ]
			ifFalse: [ self assert: each value first = $d ] ]