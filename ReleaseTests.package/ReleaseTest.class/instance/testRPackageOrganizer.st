testing - rpackage
testRPackageOrganizer
	"Ensure other tests temporary created organizers are collected"
	3 timesRepeat: [ Smalltalk garbageCollect ].

	"Now check :)"
	self 
		assert: RPackageOrganizer allInstances size = 1
		description: 'There are multiple (', RPackageOrganizer allInstances size asString, ') instances of RPackageOrganizer'.
		  
	self 
		assert: RPackageOrganizer allInstances first == RPackageOrganizer default 
		description: 'The default package organizer is the not the only instance of RPackageOrganizer'.