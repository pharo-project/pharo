instance creation
block: aBlock "<BlockContext>" stdcallArgsClass: argsProxyClass "<Alien class>" "^<Callback>"
	"Answer a Callback that evaluates aBlock and whose thunk should conform to the
	 stdcall (Pascal) calling convention with a stack size of argsProxyClass dataSize."
	<legacy>
	^self new
		block: aBlock
		thunk: (FFICallbackThunk newStdCall: argsProxyClass dataSize)
		argsProxyClass: argsProxyClass