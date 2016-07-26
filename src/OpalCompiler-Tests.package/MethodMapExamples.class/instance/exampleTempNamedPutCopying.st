examples
exampleTempNamedPutCopying
	| b |
	b := 1.
	^[ | a |
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext put: 2. thisContext tempNamed: 'b' ] value