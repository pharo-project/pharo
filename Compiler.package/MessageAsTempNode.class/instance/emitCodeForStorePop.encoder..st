code generation
emitCodeForStorePop: stack encoder: encoder
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self emitCodeForEffect: stack encoder: encoder