test - block arguments
testAccessingBlockArgumentOptimizedBlock
	" the #ifNotNil: call is an optimized block, the block argument 'x' actually a method temporary.
(OCContextTempMappingTest>>#testAccessingBlockArgumentOptimizedBlock) numTemps -> 3"

	| a b |
	a := 1.
	b := a ifNotNil: [ :x | thisContext tempNames ].
	self assert: b equals: #(#a #b #x)