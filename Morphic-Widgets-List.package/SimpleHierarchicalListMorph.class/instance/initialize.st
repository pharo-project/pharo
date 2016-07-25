initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	self initForKeystrokes.
	self
		on: #mouseMove
		send: #mouseStillDown:onItem:
		to: self