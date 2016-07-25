examples
exampleTempNamedPutTempVector2
	| b |
	b := 1.
	^[ | a |
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext put: 3. thisContext outerContext tempNamed: 'b' ] value