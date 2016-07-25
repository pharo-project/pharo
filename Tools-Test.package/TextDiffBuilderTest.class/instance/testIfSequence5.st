tests
testIfSequence5

	| patch matches nonMatches |
	patch := self patchSequenceFor: #(a b c d) and: #(c d a b).
	self assert: patch size = 6.	"lcs is ab or cd"
	matches := (patch select: [ :each | each key = #match ])
		collect: [ :each | each value first ] as: String.
	self assert: (#('ab' 'cd') includes: matches).
	self assert: (patch count: [ :each | each key = #insert ]) = 2.
	self assert: (patch count: [ :each | each key = #remove ]) = 2.
	nonMatches := #('ab' 'cd') detect: [ :each | each ~= matches ].
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: (matches includes: each value first) ]
			ifFalse: [ self assert: (nonMatches includes: each value first) ] ]