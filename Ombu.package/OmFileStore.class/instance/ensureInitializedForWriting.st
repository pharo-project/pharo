initialize-release
ensureInitializedForWriting

	writerLock ifNotNil: [ ^ self " => Already initialized" ].

	entryWriter := self newEntryWriter.
	writerLock := Semaphore forMutualExclusion.
