message performing
perform: aSymbol 
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."
	
	<reflective: #object:performMessageWith:>
	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array new: 0)