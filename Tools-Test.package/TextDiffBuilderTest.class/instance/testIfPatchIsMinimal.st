tests
testIfPatchIsMinimal

	| patch |
	patch := self patchSequenceFor: #(a a a b) and: #(a b a a).
	self assert: patch size = 5.	"lcs is aaa"
	self assert: (patch count: [ :each | each key = #match ]) = 3.
	self assert: (patch count: [ :each | each key = #insert ]) = 1.
	self assert: (patch count: [ :each | each key = #remove ]) = 1.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: each value first = $a ]
			ifFalse: [ self assert: each value first = $b ] ]