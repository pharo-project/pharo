as yet unclassified
mouseUp: anEvent
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [anEvent hand showTemporaryCursor: nil].
	self adoptPaneColor: self paneColor