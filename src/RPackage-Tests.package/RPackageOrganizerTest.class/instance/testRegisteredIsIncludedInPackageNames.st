tests
testRegisteredIsIncludedInPackageNames
	"self debug: #testRegisteredIsIncludedInPackageNames"
	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self packageOrganizer registerPackage: p1.
	self packageOrganizer registerPackage: p2.
	self packageOrganizer registerPackage: p3.
	self assert: self packageOrganizer packageNames size = 4.
	self assert: (self packageOrganizer includesPackage: p1). 
	self assert: (self packageOrganizer includesPackageNamed: p1 name). 
	{p1 . p2 . p3} do: [:each | self assert:  (self packageOrganizer packageNames includes: each name)]

