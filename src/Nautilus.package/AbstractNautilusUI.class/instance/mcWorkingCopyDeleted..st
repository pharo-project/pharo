monticello announcements
mcWorkingCopyDeleted: anAnnouncement
	| package rpackages |
	
	window ifNil: [^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	package := anAnnouncement package.
	package ifNil: [ ^ self ].
	rpackages := package packageSet packages.
	rpackages isEmptyOrNil ifTrue: [ ^ self ].
	rpackages do: [:rpackage | 
		PackagesIconsCache removeKey: rpackage ifAbsent: []].
	self updatePackageView