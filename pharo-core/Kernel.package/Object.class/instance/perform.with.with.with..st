message performing
perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."
	
	<reflective: #object:performMessageWith:>
	<primitive: 83>
	^ self perform: aSymbol
		withArguments: {firstObject . secondObject . thirdObject}