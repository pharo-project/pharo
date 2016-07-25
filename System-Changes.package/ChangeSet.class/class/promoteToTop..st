enumerating
promoteToTop: aChangeSet
	"Make aChangeSet the first in the list from now on"

	AllChangeSets remove: aChangeSet ifAbsent: [^ self].
	AllChangeSets add: aChangeSet