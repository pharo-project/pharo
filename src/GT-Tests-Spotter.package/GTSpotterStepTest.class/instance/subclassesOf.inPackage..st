private-navigation
subclassesOf: aClass inPackage: aPackageName
	^ (RPackageOrganizer default packages 
		detect: [ :each | each packageName = aPackageName ] 
		ifNone: [ ^ #() ]) classes intersection: aClass withAllSubclasses