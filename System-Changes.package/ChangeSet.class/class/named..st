enumerating
named: aName
	"Return the change set of the given name, or nil if none found."

	^ AllChangeSets
			detect: [:aChangeSet | aChangeSet name = aName]
			ifNone: [nil]