tests
testIfSequence3

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(b d c a).
	self assert: patch size = 6.	"lcs is bd"
	self assert: (patch count: [ :each | each key = #match ]) = 2.
	self assert: (patch count: [ :each | each key = #insert ]) = 2.
	self assert: (patch count: [ :each | each key = #remove ]) = 2.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('bd' includes: each value first) ]
			ifFalse: [ self assert: ('ac' includes: each value first) ] ]