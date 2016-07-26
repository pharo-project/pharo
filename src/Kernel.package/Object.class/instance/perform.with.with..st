message performing
perform: aSymbol with: firstObject with: secondObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<reflective: #object:performMessageWith:>
	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)