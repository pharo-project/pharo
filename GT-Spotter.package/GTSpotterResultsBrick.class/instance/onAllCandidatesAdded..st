event handling
onAllCandidatesAdded: anAnnouncement
	"first exit if collection is empty"
	anAnnouncement candidateLinks ifEmpty: [ ^ self ].
	self
		addItems: (anAnnouncement candidateLinks collect: [ :each | self buildItemFor: each in: anAnnouncement category ])
		to: (self categoryBrickFor: anAnnouncement category)