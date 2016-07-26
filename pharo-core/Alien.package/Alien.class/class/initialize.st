class initialization
initialize
	"Alien initialize"
	LoadedLibraries ifNil:
		[LoadedLibraries := Dictionary new].
	GCMallocedAliens ifNil:
		[GCMallocedAliens := AlienWeakTable newForOwner: self].
	WordSize := Smalltalk wordSize.
	self ensureInSpecialObjectsArray.
	self ensureNotifiedAtStartup