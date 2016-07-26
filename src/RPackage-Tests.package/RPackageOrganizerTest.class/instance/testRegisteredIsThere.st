tests
testRegisteredIsThere
	"self debug: #testRegisteredIsThere"

	| p1 p2 p3 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.

	self packageOrganizer basicRegisterPackage: p1.
	self packageOrganizer basicRegisterPackage: p2.
	self packageOrganizer basicRegisterPackage: p3.
	self assert: self packageOrganizer packageNames size = 4.
	
	{p1 . p2 . p3} do: [:each | 
		self assert:  (self packageOrganizer packageNames includes: each name)]