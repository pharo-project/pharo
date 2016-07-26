message performing
perform: selector withArguments: argArray 
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."
	
	<reflective: #object:performMessageWithArgs:>
	<primitive: 84>
	^ self perform: selector withArguments: argArray inSuperclass: self class