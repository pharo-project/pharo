*Monticello-RPackage
updateAfterNewMCPackageRegistred: anAnnouncement
	"User create a new package, MCWorkingCopy propagates changes. We react accordingly."
	|  name |
	
	name := anAnnouncement package name.
	(self packageExactlyMatchingExtensionName: name) 
		ifNil: [ self ensureExistAndRegisterPackageNamed: name ]