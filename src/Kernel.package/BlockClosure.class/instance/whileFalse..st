controlling
whileFalse: aBlock 
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the argument, aBlock, as long as the value of the receiver is false."

	self value ifFalse: [ aBlock value. self whileFalse: aBlock ]