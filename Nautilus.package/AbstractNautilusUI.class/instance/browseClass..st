plugins announcements
browseClass: aClass
	self selectedPackage: (self parentOfClass: aClass).
	self selectedClass: aClass.
	self updatePackageGroupAndClassList