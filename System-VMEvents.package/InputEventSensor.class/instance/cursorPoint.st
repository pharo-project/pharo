cursor
cursorPoint
	"Answer a Point indicating the cursor location."

	"Fetch the next event if any to update state.
	Makes sure that the old polling methods consume events
	self nextEvent."

	^ mousePosition