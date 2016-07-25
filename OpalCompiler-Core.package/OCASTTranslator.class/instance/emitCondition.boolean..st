inline messages factored
emitCondition: args boolean: aBoolean
	"emits the jumps so that one of the 2 blocks in args is evaluated depending on boolean"
	
	methodBuilder jumpAheadTo: #else if: aBoolean.
	self visitInlinedBlockNode: args first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	self visitInlinedBlockNode: args last.
	methodBuilder jumpAheadTarget: #end.