accessing
adoptPaneColor: paneColor
	"Set the color."
	
	(paneColor notNil and: [self trackPaneColor])
		ifTrue: [self color: paneColor].
	super adoptPaneColor: paneColor