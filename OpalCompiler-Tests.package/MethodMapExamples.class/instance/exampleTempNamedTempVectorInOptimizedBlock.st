examples
exampleTempNamedTempVectorInOptimizedBlock
"temp a is in a temp vector and is accessed from with in the optimized ifTrue:ifFalse: block. But 
the definition is not in the outer block, but in the method scope"
	| result a |
	[ a := 1.
	result := true
		ifTrue: [ 1 + ((DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'a' in: thisContext) ]
		ifFalse: [ 4 ] ] value.
	^ result