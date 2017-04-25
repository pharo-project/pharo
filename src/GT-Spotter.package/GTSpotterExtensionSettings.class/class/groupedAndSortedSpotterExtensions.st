settings
groupedAndSortedSpotterExtensions
	"Return a list of associations in which:
		the key is a class
		the value is the list of spotter extensions.
	The returned list is ordered according to the class name"

	| extensions groupedExtensions |
	extensions := GTSpotter spotterExtendingMethods.
	groupedExtensions := (extensions groupedBy: #methodClass) associations.
	^ groupedExtensions asSortedCollection: [ :a1 :a2 | a1 key name < a2 key name ]. 