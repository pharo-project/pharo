tests
testPackageOfClassMethodFromTraitExtensionIsExtendingPackage
	"The package of a method defined in atrait but package in another package than the extended trait is the 
	package containing the extension."
	
	"The package of a method in A1 (which is coming from the trait T1 used by A1) is the package of T1"
	self assert: ((a1>>#traitMethodExtendedFromP4) packageFromOrganizer: self packageClass organizer) = p4.
	"The package of a method in A1 (which is coming from the trait T1 used by A1 but extended in package T2) is the package of T2"
	self assert: ((a1>>#traitMethodExtendedFromP6) packageFromOrganizer: self packageClass organizer) = p6.