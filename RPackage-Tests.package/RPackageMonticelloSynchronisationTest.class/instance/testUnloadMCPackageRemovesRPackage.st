tests - operations on MCPackages
testUnloadMCPackageRemovesRPackage
	"test that when we remove a MC Package, the corresponding RPackage is removed from the organizer"
	|XPackage class|

	MCWorkingCopy forPackage: (MCPackage new name: 'XXXXX').
	(self allManagers detect: [:each | each packageName = 'XXXXX']) unload.

	self flag: #fixme."See comment in RPackageOrganizer>>update:"
	self deny: (self organizer includesPackageNamed: #XXXXX).