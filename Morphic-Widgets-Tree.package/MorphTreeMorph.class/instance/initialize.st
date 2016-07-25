initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	columnColors := Array 
		with: Color transparent 
		with: Color transparent.
	rowColors := Array 
		with: self theme backgroundColor 
		with: self theme backgroundColor.
	self borderWidth: 0.

	
