monticello menus behavior
createMCPackageFor: aPackage
	| name |
	name := aPackage name.
	name isEmptyOrNil ifFalse: [ MCWorkingCopy forPackage: (MCPackage new name: name) ]