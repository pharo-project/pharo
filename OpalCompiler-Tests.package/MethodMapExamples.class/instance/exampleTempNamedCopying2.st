examples
exampleTempNamedCopying2
	| b |
	b := 1.
	^[ | a |
		a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value