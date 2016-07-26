code generation
emitCodeForBlockValue: stack encoder: encoder
	"Generate code for evaluating the last statement in a block"
	^self emitCodeForValue: stack encoder: encoder