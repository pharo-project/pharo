simulation
runSimulated: aBlock
	"Simulate the execution of the argument, current. Answer the result it 
	returns."

	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [:ignored |]

	"Context runSimulated: [Pen new defaultNib: 5; go: 100]"