accessing class hierarchy
commentInventory
	"Answer a string with a count of the classes with and without comments 
	for all the classes in the package of which this class is a member."

	"Morph commentInventory"

	^ self environment organization commentInventory: (self category copyUpTo: $-), '*'