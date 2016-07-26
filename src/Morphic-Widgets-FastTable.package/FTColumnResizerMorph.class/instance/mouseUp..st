Polymorph-Widgets
mouseUp: anEvent
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	self canResizeColumn ifFalse: [^ self].
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [anEvent hand showTemporaryCursor: nil].
	self updateFromEvent: anEvent.
	traceMorph ifNotNil: [traceMorph delete. traceMorph := nil].
	self adoptPaneColor: self paneColor.
	self triggerEvent: #mouseUp