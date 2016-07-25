I install callbacks as functions (to be used with ffiCall: later)

I can register any callback with a name, the callback thunk will be used as an entry point for this function.

I implement the option:  optCallbackCall 
	
Example: 
========
callAbsoluteMethod
	 |  callback |

	callback := FFICallback 
		signature: #(double (double x)) 
		block: [ :x | x abs ].
	FFICallbackFunctionResolution register:  callback as:  #absolute.
	self absolute: 42.0.

absolute: aNumber
	self ffiCall: #(double absolute (double aNumber)) options: #(+optCallbackCall)	
