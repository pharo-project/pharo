examples
exampleTempNamedTempVectorNestedBlock
	 | a |
      a  := 1.
	^[| b |
		b := a.
		[  
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value] value