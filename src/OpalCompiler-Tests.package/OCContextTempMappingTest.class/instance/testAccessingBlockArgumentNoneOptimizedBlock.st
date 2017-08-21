test - block arguments
testAccessingBlockArgumentNoneOptimizedBlock
	" the #in: call is not optimized, the block argument 'x' is a real block local temporary (argument).
(OCContextTempMappingTest>>#testAccessingBlockArgumentNoneOptimizedBlock) numTemps -> 2"

	| a b |

	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].

	a := 1.
	b := a in: [ :x | thisContext tempNames ].
	self assert: b equals:#(a b x)