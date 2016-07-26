private
storeAt: offset inTempFrame: aContext
	"This message had to get sent to an expression already on the stack
	as a Block argument being accessed by the debugger.
	Just re-route it to the temp frame."
	^ aContext tempAt: offset put: self