tests
testMethodPackageFromObsoleteClass
	"self debug: #testMethodPackageFromObsoleteClass"
	
	| pack method foo | 
	[
	pack := self createNewPackageNamed: 'P1'.
	foo := self createNewClassNamed: #FooForTest inPackage: pack.
	foo compile: 'bar ^42'.
	method := foo>>#bar.

	foo obsolete.
	self assert: (method packageFromOrganizer: pack organizer) equals: foo package.
	] ensure: [ 
		foo ifNotNil: [ 
			foo setName: foo originalName.
			foo removeFromSystem.
			].
		Smalltalk globals removeKey: #FooForTest  ifAbsent:  [].
	]