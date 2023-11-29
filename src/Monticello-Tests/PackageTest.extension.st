Extension { #name : 'PackageTest' }

{ #category : '*Monticello-Tests' }
PackageTest >> testMcWorkingCopy [

	| rPackage |
	rPackage := self organizer ensurePackage: #Test1.
	self assert: rPackage mcWorkingCopy identicalTo: (MCWorkingCopy forPackageNamed: #Test1)
]
