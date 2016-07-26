action
function: functionSignature module: moduleNameOrLibrary
	| sender ffiMethod |
	
	sender := context ifNil: [thisContext sender sender].

	"Build new method"
	ffiMethod := self newBuilder build: [ :builder | 
		builder
			signature: functionSignature;  
			sender: sender;
			module: moduleNameOrLibrary ].
	ffiMethod 
		selector: sender selector;
		methodClass: sender methodClass.		
	"Replace with generated ffi method, but save old one for future use"
	ffiMethod 
		propertyValueAt: #ffiNonCompiledMethod 
		put: (sender methodClass methodDict at: sender selector).
	sender methodClass methodDict 
		at: sender selector
		put: ffiMethod.
	"Register current method as compiled for ffi"
	FFIMethodRegistry uniqueInstance registerMethod: ffiMethod.
	"Resend"
	sender return: (sender receiver 
		withArgs: sender arguments 
		executeMethod: ffiMethod)