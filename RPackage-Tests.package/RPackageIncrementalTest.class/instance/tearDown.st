setup
tearDown
	
	| logging |
	createdPackages  do: [:each | self removePackage: each name].
	"just remove package from package organizer dictionary"
	
	createdPackages  do: [:each |
		|mCPackage|
		mCPackage := self allManagers 
							detect: [:mcPackage | mcPackage packageName = each packageName asString] 
							ifNone: [nil].
		mCPackage ifNotNil: [mCPackage unregister].
		each extendedClasses do: [ :extendedClass|
			self packageClass organizer
		 		unregisterExtendingPackage: each forClass: extendedClass.]].
	"all ***extending*** classes the packages are also unregistered from PackageOrganizer"									
	(createdClasses reject: [:c| c isObsolete]) do: [:cls| 
		"(RPackage organizer includesPackageBackPointerForClass: cls) 
			ifTrue: [cls package unregisterClass: cls.]. 
		when RPackageOrganizer was not looking at system event we had to do the commented actions"
		logging := false.
		cls removeFromSystem: logging.  
		"not logging so no event are raised"
		"but this also means that the consistency cannot be ensured by internal system announcer too."
		].					
	createdCategories do: [:each | 
		|mCPackage|
		SystemOrganization removeCategory: each.
		 ].
	super tearDown.