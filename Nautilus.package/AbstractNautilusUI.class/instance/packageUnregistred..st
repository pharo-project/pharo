system announcements
packageUnregistred: anAnnouncement
	| package |
	package := anAnnouncement package.
	self selectedPackage = package
		ifTrue: [ self selectedPackage: nil ].
	self packageWidget selectPackage: package.
	window isDisplayed
		ifFalse: [ ^ self ].
	self updatePackageView