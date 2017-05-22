initialization
initialize
	super initialize.
	commitDictionary := Dictionary new.
	self initializeAnnouncer.

	loadedCode := IceLoadedCode repository: self.
