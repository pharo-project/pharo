enumerating
removeChangeSet: aChangeSet
	"Remove the given changeSet.  Caller must assure that it's cool to do this"

	AllChangeSets remove: aChangeSet ifAbsent: [].
	aChangeSet wither
