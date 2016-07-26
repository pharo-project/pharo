monticello announcements
mcPackageModified: anAnnouncement
	" handled when a package become dirty "
	
	| rpackages |
	window ifNil: [^ self ].
	window isDisplayed ifFalse: [ ^ self ].

	rpackages := anAnnouncement package packageSet packages.
	rpackages isEmptyOrNil ifTrue: [ ^ self ].
	(self model packages includesAnyOf: rpackages )
		ifTrue: [ 
			rpackages do: [:rpackage | PackagesIconsCache removeKey: rpackage ifAbsent: []].
			self updatePackageView ]