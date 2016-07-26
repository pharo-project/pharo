event handling
mouseUp: anEvent
	"Change the cursor back to normal if necessary."
	
	(self bounds containsPoint: anEvent cursorPoint) ifFalse: [
		anEvent hand showTemporaryCursor: nil.
		self
			setDefaultColors;
			changed]