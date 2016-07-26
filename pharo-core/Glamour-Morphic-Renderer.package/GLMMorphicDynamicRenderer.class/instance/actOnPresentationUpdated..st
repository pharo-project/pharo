rendering
actOnPresentationUpdated: anAnnouncement
	container removeAllMorphs.
	container 
		addMorph: (self renderObject: anAnnouncement presentation currentPresentation) 
		fullFrame: (LayoutFrame identity) 