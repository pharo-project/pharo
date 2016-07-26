instance creation
hasPackageNamed: aPackageName

	^ self allManifestClasses 
		anySatisfy: [:each | each category = aPackageName  ]
	