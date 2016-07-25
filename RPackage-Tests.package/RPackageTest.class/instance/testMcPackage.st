tests-MC
testMcPackage

	| rPackage |
	rPackage := self organizer registerPackageNamed: #Test1.
	self assert: rPackage mcPackage equals: (MCPackage new name: #Test1).