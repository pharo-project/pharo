initialization
tearDown
	| aPackage |
	aPackage := RPackageOrganizer default packageNamed:#'Hermes-Tests-External-Package2' ifAbsent: nil.
	aPackage ifNotNil: #removeFromSystem.
	super tearDown.