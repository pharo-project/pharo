examples
exampleTempNamedTempCopyingNestedBlock
	
	^[| b |
		b := 1.
		[   | a |
		 a := 2.
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value] value