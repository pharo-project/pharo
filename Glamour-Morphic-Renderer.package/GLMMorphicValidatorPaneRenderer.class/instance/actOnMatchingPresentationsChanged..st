rendering
actOnMatchingPresentationsChanged: anAnnouncement 
	paneMorph	removeAllMorphs.
	self renderMorphFor: anAnnouncement pane.
	self renderer window fullBounds