rendering
actOnPaneAdded: ann
	container removeMorph: container submorphs last.
	self addMorphFromObject: ann pane toContainer: container.