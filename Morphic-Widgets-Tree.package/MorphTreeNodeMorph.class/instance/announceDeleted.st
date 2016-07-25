announcements
announceDeleted

	complexContents ifNotNil: [ complexContents removeDependent: self ].
	
	super announceDeleted.