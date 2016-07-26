tests
testPackageOfTraitMethodIsTraitPackage
	"The package of a trait method is the package of its trait."

	self assert: ((a1 >> #traitMethodDefinedInP5) packageFromOrganizer: self packageClass organizer) = p5.
	self assert: ((a1 >> #traitMethodDefinedInP5)packageFromOrganizer: self packageOrganizerClass default) = p5.
	self assert: ((a1>>#traitMethodDefinedInP4) packageFromOrganizer: self packageClass organizer) = p4.