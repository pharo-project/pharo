spec creation
createPackageSpec: baseName
	^ (project packageSpec)
		name: baseName;
		yourself