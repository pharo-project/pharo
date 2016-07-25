primitives-callbacks
primReturnFromContext: callbackContext "<ContextPart>" through: jmpbuf "<Integer>" "^not reached"
	"Return a value, as described by the receiver, to some C callee that has made a
	callback.   Reestablish the interpreter's state prior to the callback so that either
	the callout can return or subsequent callbacks can occur.  callbackContext is an
	activation of invokeCallback:stack:registers:jmpbuf:.  Its sender is the interpreter's
	state prior to the callback"
	<primitive: 'primReturnFromContextThrough' module: 'IA32ABI' error: errorCode>
	self primitiveFailed