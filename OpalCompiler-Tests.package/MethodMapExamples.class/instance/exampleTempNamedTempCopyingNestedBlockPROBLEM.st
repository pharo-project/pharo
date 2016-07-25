examples
exampleTempNamedTempCopyingNestedBlockPROBLEM
	 | a |
		 a := 2. 
	^[| b | 
		b := 1.
		[  
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value] value