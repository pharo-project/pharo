protocol-events
whenTextAreaSelectionChanged: anAnnouncement 
	firstIndex := self textArea markIndex min: self textArea pointIndex.
	lastIndex := self textArea pointIndex max: self textArea markIndex.
	self computeVertices.
	self announcer announce: RubTextSetInModel.
