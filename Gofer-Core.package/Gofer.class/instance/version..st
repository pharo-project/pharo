references
version: aString
	"Add the version aString to the receiver. aString is a version name as it appears in the Monticello Repository Browser, something like 'Magritte-Seaside-lr.334'. Gofer will try to resolve this name to one of the configured repositories before loading the code."

	references addLast: (GoferVersionReference name: aString)