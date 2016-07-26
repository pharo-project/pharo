utilties
runMatcher: aMatcher with: aString expect: aBoolean withSubexpressions: anArray
	| copy got |
	copy := aMatcher
		copy: aString
		translatingMatchesUsing: [ :each | each ].
	self 
		assert: copy = aString
		description: 'Copying: expected ' , aString printString , ', but got ' , copy printString.
	got := aMatcher search: aString.
	self
		assert: got = aBoolean 
		description: 'Searching: expected ' , aBoolean printString , ', but got ' , got printString.
	(anArray isNil or: [ aMatcher supportsSubexpressions not ])
		ifTrue: [ ^ self ].
	1 to: anArray size by: 2 do: [ :index |
		| sub subExpect subGot |
		sub := anArray at: index.
		subExpect := anArray at: index + 1.
		subGot := aMatcher subexpression: sub.
		self
			assert: subExpect = subGot
			description: 'Subexpression ' , sub printString , ': expected ' , subExpect printString , ', but got ' , subGot printString ]