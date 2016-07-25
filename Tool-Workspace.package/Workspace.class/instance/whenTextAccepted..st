update
whenTextAccepted: anAnnouncement
	self lastContents size > self maxKeptContents
		ifTrue: [ self lastContents removeFirst ].
	self lastContents add: textModel getText copy