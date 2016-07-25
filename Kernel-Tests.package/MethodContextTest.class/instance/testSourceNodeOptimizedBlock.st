tests - ast mapping
testSourceNodeOptimizedBlock
	"When asking for the sourceNode of a block that is not there at runtime, we nevertheless what to 
	get the corresponding ASTnode of the inlined Block"
	
	| sourceNode |
	
	sourceNode := Smalltalk compiler evaluate: '1 to: 1 do: [ :i | ^ thisContext sourceNode ]'.
	self assert: sourceNode isBlock.