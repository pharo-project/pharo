examples
exampleSimpleTemp
	| b |
	b := 1.
	^(DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext