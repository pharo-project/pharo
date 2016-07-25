services
newChangeSet
	"Prompt the user for a name, and establish a new change set of
	that name (if ok), making it the current changeset.  Return nil
	of not ok, else return the actual changeset."

	| newName newSet |
	newName := UIManager default
		request: 'Please name the new change set:'
		initialAnswer: self defaultName.
	newName isEmptyOrNil ifTrue:
		[^ nil].
	newSet := self basicNewChangeSet: newName.
	newSet ifNotNil:
		[self  newChanges: newSet].
	^ newSet