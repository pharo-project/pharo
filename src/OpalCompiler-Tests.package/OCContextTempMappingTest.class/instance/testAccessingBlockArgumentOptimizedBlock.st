test - block arguments
testAccessingBlockArgumentOptimizedBlock
	" the #ifNotNil: call is an optimized block, the block argument 'x' actually a method temporary.
(OCContextTempMappingTest>>#testAccessingBlockArgumentOptimizedBlock) numTemps -> 3"

	| a b |
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].

	a := 1.
	b := a ifNotNil: [ :x | thisContext tempNames ].
	self assert: b equals: #(#a #b #x)