private events
modifiers
	"modifier keys; bitfield with the following entries:
		1	-	shift key
		2	-	ctrl key
		4	-	(Mac specific) option key
		8	-	Cmd/Alt key"

	"Fetch the next event if any to update state.
	Makes sure that the old polling methods consume events"

"	self nextEvent."
	

	^modifiers