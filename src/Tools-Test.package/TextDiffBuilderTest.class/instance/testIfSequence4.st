tests
testIfSequence4

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(d b c a).
	self assert: patch size = 6.	"lcs is bc"
	self assert: (patch count: [ :each | each key = #match ]) = 2.
	self assert: (patch count: [ :each | each key = #insert ]) = 2.
	self assert: (patch count: [ :each | each key = #remove ]) = 2.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('bc' includes: each value first) ]
			ifFalse: [ self assert: ('ad' includes: each value first) ] ]