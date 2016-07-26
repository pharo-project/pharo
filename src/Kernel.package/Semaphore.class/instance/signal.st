communication
signal
	"Primitive. Send a signal through the receiver. If one or more processes 
	have been suspended trying to receive a signal, allow the first one to 
	proceed. If no process is waiting, remember the excess signal. Essential. 
	See Object documentation whatIsAPrimitive."

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals := excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"

