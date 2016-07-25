tests-MC
testMcWorkingCopy

	| rPackage |
	rPackage := self organizer registerPackageNamed: #Test1.
	self assert: rPackage mcWorkingCopy equals: (MCWorkingCopy forPackage: (MCPackage new name: #Test1)).