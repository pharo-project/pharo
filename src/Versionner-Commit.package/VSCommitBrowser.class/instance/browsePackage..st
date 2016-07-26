actions
browsePackage: aPackage
	| package |
	package := RPackageOrganizer default packageNamed: aPackage packageName.
	Smalltalk tools browser openOnPackage: package