testing - rpackage
testUnpackagedClasses
	| unpackagedClasses |
	unpackagedClasses := Smalltalk allClassesAndTraits select: [:each | 
		(RPackageOrganizer default packageOf: each) packageName = RPackage defaultPackageName ].
	self 
		assert: unpackagedClasses isEmpty 
		description: (String streamContents: [ :s|
			s nextPutAll: 'Found the following unpackaged classes: '.
			unpackagedClasses
				do: [ :cls| s tab print: cls ]
				separatedBy: [ s cr ]])
