control
blockTo: end
	"Decompile a range of code as in statementsTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block