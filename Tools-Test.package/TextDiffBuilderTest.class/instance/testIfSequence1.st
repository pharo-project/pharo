tests
testIfSequence1

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(d c b a).
	self assert: patch size = 7.	"lcs is any one letter sequence"
	self assert: (patch count: [ :each | each key = #match ]) = 1.
	self assert: (patch count: [ :each | each key = #insert ]) = 3.
	self assert: (patch count: [ :each | each key = #remove ]) = 3.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: each value first = $d ]
			ifFalse: [ self assert: ('abc' includes: each value first) ] ]