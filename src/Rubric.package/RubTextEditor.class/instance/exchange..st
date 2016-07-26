editing keys
exchange: aKeyboardEvent
	"Exchange the current and prior selections."

	self closeTypeIn.
	self exchange.
	^true