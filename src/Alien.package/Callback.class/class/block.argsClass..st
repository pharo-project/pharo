instance creation
block: aBlock "<BlockContext>" argsClass: argsProxyClass "<Alien class>" "^<Callback>"
	"Answer a Callback that evaluates aBlock and whose thunk should conform to the C calling convention."
	<legacy>
	^self new
		block: aBlock
		thunk: FFICallbackThunk newCCall
		argsProxyClass: argsProxyClass