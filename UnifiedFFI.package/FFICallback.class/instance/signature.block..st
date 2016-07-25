initialization
signature: signature "<String>" block: aBlock "<BlockClosure>"
	functionSpec := self newParser parseAnonymousFunction: signature.
	block := aBlock.
	"declare the real callback"
	callback := Callback forCallbackEvaluator: self.
	"keeping the thunk here to easy the FFI method compilation"
	thunk := callback thunk.