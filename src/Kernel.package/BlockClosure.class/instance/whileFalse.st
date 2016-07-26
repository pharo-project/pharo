controlling
whileFalse
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the receiver, as long as its value is false."
 
	self value ifFalse: [ self whileFalse ]