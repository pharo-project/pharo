compiling
compile: code classified: heading 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary under 
	the classification indicated by the second argument, heading. nil is to be 
	notified if an error occurs. The argument code is either a string or an 
	object that converts to a string or a PositionableStream on an object that 
	converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil