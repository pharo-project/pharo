event handling
onCandidateAdded: anAnnouncement
	self
		addItem: (self buildItemFor: anAnnouncement candidateLink in: anAnnouncement category)
		to: (self categoryBrickFor: anAnnouncement category)