spec creation
createVersionSpec: versionString
	| versionSpec |
	versionSpec := project versionSpec.
	versionSpec versionString: versionString.
	^ versionSpec