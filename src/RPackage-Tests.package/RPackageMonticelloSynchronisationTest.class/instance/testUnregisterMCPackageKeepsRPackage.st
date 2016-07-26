tests - operations on MCPackages
testUnregisterMCPackageKeepsRPackage
	"test that when we remove a MC Package, the corresponding RPackage is removed from the organizer"
	
	|XPackage class|
	MCWorkingCopy forPackage: (MCPackage new name: 'XXXXX').
	(self allManagers detect: [:each | each packageName = 'XXXXX']) unregister.
	
	self assert: (self organizer includesPackageNamed: #XXXXX).