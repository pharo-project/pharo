system announcements
classRecategorized: anAnnouncement
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	self selectedPackage ifNotNil:[:selection |
	({ anAnnouncement oldCategory. anAnnouncement newCategory }
		anySatisfy: [ :each | selection includesCategory: each ])
			ifTrue: [ self updatePackageGroupAndClassList ]]