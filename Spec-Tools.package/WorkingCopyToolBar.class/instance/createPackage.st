initialization
createPackage
	| name |
	name := UIManager default request: 'Name of package:'.
	name isEmptyOrNil
		ifTrue: [ ^ self ].
	RPackageOrganizer default registerPackageNamed: name.
	MCWorkingCopy forPackage: (MCPackage new name: name)