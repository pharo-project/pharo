Extension { #name : #LargePositiveIntegerTest }

{ #category : #'*Kernel-Tests-Extended' }
LargePositiveIntegerTest >> testLargeSqrtFloor [
	"This test fails if a careless implementation naivly factors out the power of two (remove the trailing zeroes up to lowBit).
	This was the case in a previous implementation, so this is a non regression test."

	| large root |
	large := (SmallInteger maxVal << 100) + 1 << 100.
	root := large sqrtFloor.
	self assert: root squared <= large.
	self assert: (root + 1) squared > large
]

{ #category : #'*Kernel-Tests-Extended' }
LargePositiveIntegerTest >> testMultDicAddSub [
	| n f f1 |
	n := 100.
	f := 100 factorial.
	f1 := f * (n + 1).
	n timesRepeat: [ f1 := f1 - f ].
	self assert: f1 equals: f.
	n timesRepeat: [ f1 := f1 + f ].
	self assert: f1 // f equals: n + 1.
	self
		assert: f1 negated
		equals: (Number readFrom: '-' , f1 printString)
]
