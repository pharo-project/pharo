tests - operations on MCPackages
testAddMCPackageCreatesAPackage
	"test that when we create a MCPackage, a corresponding package is created"
	
	MCWorkingCopy forPackage: (MCPackage new name: #Zork).
	self assert: (self organizer includesPackageNamed: #Zork).
		
					  