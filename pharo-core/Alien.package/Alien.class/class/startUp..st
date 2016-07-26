system startup
startUp: resuming
	resuming ifTrue:
		[LoadedLibraries := Dictionary new: LoadedLibraries size.
		"Any addresses in the finalization registry at the time the image
		 comes up in a new session MUST NOT be finalized and should
		 immediately be discarded. These addresses are no longer valid."
		 GCMallocedAliens ifNotNil:
			[:gcMallocedAliens| gcMallocedAliens removeAll].
		"The io lock is off in the new session, so we shut down the process if it's running."
		ProcessIoEvents := false.
		WordSize := Smalltalk wordSize]