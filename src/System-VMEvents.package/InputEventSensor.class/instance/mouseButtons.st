private events
mouseButtons
	"button state; bitfield with the following entries:
		1	-	yellow (e.g., right) button
		2	-	blue (e.g., middle) button
		4	-	red (e.g., left) button
		[all other bits are currently undefined]"

	"Fetch the next event if any to update state.
	Makes sure that the old polling methods consume events"
	self nextEvent.
	
	^mouseButtons