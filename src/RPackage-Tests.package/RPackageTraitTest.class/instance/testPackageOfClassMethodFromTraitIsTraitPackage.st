tests
testPackageOfClassMethodFromTraitIsTraitPackage

	"test that a class method coming from a trait is packaged in the trait package"
	self assert: ((a1>>#traitMethodDefinedInP4) packageFromOrganizer: self packageClass organizer) = p4.
	self assert: ((a1>>#traitMethodDefinedInP5) packageFromOrganizer: self packageOrganizerClass default) = p5.