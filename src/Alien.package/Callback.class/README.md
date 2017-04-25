Callbacks encapsulate callbacks from the outside world.  They allow Smalltalk blocks to be evaluated and answer their results to external (e.g. C) callees.  Callbacks are created with signature:block:, e.g.

	cb := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].

and passed through the FFI by passing their pointer, e.g.

	self qui: data ck: data size so: 8 rt: cb pointer

When the callback is made, the system arranges that the block is invoked with the arguments as defined by the signature, and the result of the block passed back, again as defined by the signature.  See methods in the signatures protocol in subclasses of Callback for signature methods that decode the C stack and registers to invoke a callback with parsed arguments.  See Callback>>valueInContext: and subclass implementations for the evaluation of the signature method that invokes the callback block with correctly parsed arguments.  See Alien class>>invokeCallbackContext: for the entry-point for callbacks into the system from the VM.

Instance Variables:
block <BlockClosure> - The Smalltalk code to be run in response to external code invoking the callback.
thunk <FFICallbackThunk> - the wrapper around the machine-code thunk that initiates the callback and whose address should be passed to C
evaluator <Symbol> - the selector of the marshalling method to use; see methods in the signatures protocol in subclasses of Callback.
numEvaluatorArgs <Integer> - the arity of evaluator
argsProxyClass <Alien subclass> - legacy; unused; the wrapper around the thunk's incoming stack pointer, used to extract arguments from the stack.

Class Variables:
ThunkToCallbackMap <Dictionary of: thunkAddress <Integer> -> callback <Callback>> - used to lookup the Callback associated with a specific thunk address on callback.  See FFICallbackThunk.
ABI <String> - the name of the current ABI

Class Instance Variables
concreteClass <Callback subclass> - the concrete class for callbacks on the current platform, or nil if one doesn't yet exist.

Implementation:
The way that it works is in two parts
- on callback the VM passes up a pointer to a structure from which all arguments, stacked and in registers (because the VM has copied any register args into the struct) can be accessed, and through which the result can be returned.
- the image level provides marshalling methods that match the signature in the callback.  Marshalling methods belong in concrete subclasses, one subclass for each ABI.

So e.g. with a callback of
		Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign]
the marshalling methods are in one of Callback's concrete subclasses signatures protocol, for example

CallbackForIA32>>voidstarvoidstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))

where spAlien is an Alien pointing to a VMCallbackContext32.

For ARM support, where there the first four integer arguments are passed in registers, we can use

CallbackForARM32>>voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))

The selector of the method doesn't matter, providing it doesn't conflict with any other, except for the number of arguments.  What's important is the pragma which defines the signature and the ABI for which this is a valid marshalling method.  Support for callee pop callbacks (Pascal calling convention such as the Win32 stdcall: convention) are supported using the <calleepops: N> pragma which specifies how many bytes to pop.

When a callback is instantiated, Callback introspects to find the marshalling method that matches the signature for the current ABI.  If one doesn't already exist you can write one.  Hopefully we'll write an ABI compiler that will automatically generate these marshalling methods according to the platform's ABI, but for now its a manual process.; at least it's open and flexible.  When the callback is invoked the evaluator is performed with the current callbackContext and pointer(s) to the arguments.  There is a 32-bit and a 64-bit callback context, and it can have a stack pointer, integer register args and floating point register args, so it's general enough for any callback.

To pass back the result, a value is assigned into the struct via the accessor in the marshalling method and control returns to teh point where teh callback comes in, and this uses a primitive to return.  Inside the callbackContext is a jmpbuf from a setjmp.  The primitive longjmp's back to the entry point in the VM which extracts the result and the code for the kind of result and returns.  See Callback class>>invokeCallbackContext: