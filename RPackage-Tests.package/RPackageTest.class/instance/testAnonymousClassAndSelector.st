tests
testAnonymousClassAndSelector
	"jgeidel test case from issue 12628."

	"Make sure we don't have a registration or a package for the method."

	| ghost method uPackage |
	ghost := Object newAnonymousSubclass.
	uPackage := self organizer packageNamed: #'_UnpackagedPackage'.
	method := ghost compiler compile: 'rpackagetest'.
	ghost addSelector: #rpackagetest withMethod: method.
	self deny: (uPackage includesDefinedSelector: #rpackagetest ofClass: ghost).
	self deny: (self organizer packageOfClassNamed: ghost name) notNil.
	ghost organization classify: #rpackagetest under: '*rpackagetest'