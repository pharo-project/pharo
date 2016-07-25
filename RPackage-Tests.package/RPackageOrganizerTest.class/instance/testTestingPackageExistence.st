tests extending
testTestingPackageExistence

	| extensionCategory package existingPackage |
	
	self skip. "This tests seems to not have sense anymore (because creating of duplicated packages is forbidden). 
 	Loot at RPackageOrganizerTest>>#testRegisterPackageConflictWithPackageTag"
	
	existingPackage := (RPackage named: 'RPackage-Tests') register; yourself.
	extensionCategory := existingPackage name, '-subcategory'.
	package := RPackage named: extensionCategory.
	packageOrganizer := package organizer.
	self deny: (packageOrganizer includesPackageNamed: extensionCategory).

	packageOrganizer checkPackageExistsOrRegister: extensionCategory.

	self deny: (packageOrganizer includesPackageNamed: extensionCategory).