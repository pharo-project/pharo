message performing
perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."
	
	<reflective: #object:performMessageWith:>
	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: anObject)