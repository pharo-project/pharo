brick-morph-mouse-events
mouseLeave: evt
	super mouseLeave: evt.
	^ self deselect: evt