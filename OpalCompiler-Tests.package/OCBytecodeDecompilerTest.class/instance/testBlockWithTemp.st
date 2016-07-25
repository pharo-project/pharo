tests - blocks
testBlockWithTemp
	| method ir |
	method := ClassTestCase>>#targetClass.
	
	ir := method ir.
	
	"the block sequence of the block with the temp should not have
	a pushNil, as this is inserted later again"
	self assert: (ir allSequences at: 5) first isPushLiteral not.