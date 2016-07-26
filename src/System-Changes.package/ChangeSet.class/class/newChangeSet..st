services
newChangeSet: aName
	"Makes a new change set called aName, add author full name to try to
	ensure a unique change set name."

	| newName |
	newName := aName , '.' , Author fullName.
	^ self basicNewChangeSet: newName