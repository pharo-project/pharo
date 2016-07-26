rendering
actOnMatchingPresentationChanged: anAnnouncement
	container removeAllMorphs.
	self renderContainerFor: anAnnouncement pane.
	self renderer window
		ifNotNil: [ 
			self flag: 'this check is fishy'.
			self renderer window fullBounds ]