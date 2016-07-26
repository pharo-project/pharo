rendering
actOnUpdatedPresentation: anAnnouncement
	treeMorph model updateRoots.
	self flag: 'Calling update list explicitly here is a hack. This should be solvable by the model through a notification mechanism'.
	treeMorph updateList