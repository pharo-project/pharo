tests
testReusingConverterShouldReplaceOldData
	| buf |
	buf := encoder convertChar: 223 asCharacter.
	
	
	self assert: (buf first:3)   equals: #[195 159 0 ].

	buf := encoder convertChar: $b.
	self assert: (buf first:2)   equals: #[98 0 ]
