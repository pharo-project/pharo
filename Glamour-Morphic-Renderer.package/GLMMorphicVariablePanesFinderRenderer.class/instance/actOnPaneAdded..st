rendering
actOnPaneAdded: ann
	self addMorphFromObject: ann pane toContainer: container.
	scrollPane hScrollbar glmAnimateValue: 1.0 duration: 2000