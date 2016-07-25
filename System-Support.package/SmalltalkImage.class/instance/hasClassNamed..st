class and trait names
hasClassNamed: aString
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned."

	^ globals hasClassNamed: aString