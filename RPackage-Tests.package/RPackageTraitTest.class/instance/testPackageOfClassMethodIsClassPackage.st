tests
testPackageOfClassMethodIsClassPackage
	"The package of a local method (not defined in a trait) is the package of its class"
	
	self assert: ((a1>>#localMethodDefinedInP1) packageFromOrganizer: self packageClass organizer) = p4.
	self assert: ((a1>>#anotherLocalMethodDefinedInP1) packageFromOrganizer: self packageOrganizerClass default) = p4.
	self assert: ((a1>>#anotherLocalMethodDefinedInP1) packageFromOrganizer: self packageClass organizer) = p4.