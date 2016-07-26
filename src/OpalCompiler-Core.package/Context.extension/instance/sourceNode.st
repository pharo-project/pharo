*OpalCompiler-Core
sourceNode
	"Return the source node of the method or the block corresponding to the receiver"
	^ (method sourceNodeForPC: self neighborPCWithCorrectMapping) enclosingMethodOrBlockNode
	"Uncomment the following once the pc->AST mapping is fixed"
	"^ (method sourceNodeForPC: (pc ifNil: [ self startpc ])) enclosingMethodOrBlockNode"