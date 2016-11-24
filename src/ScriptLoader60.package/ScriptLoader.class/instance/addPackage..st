mc related utils
addPackage: aString
	| workingCopy |
	RPackageOrganizer default registerPackageNamed: aString.
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: aString)