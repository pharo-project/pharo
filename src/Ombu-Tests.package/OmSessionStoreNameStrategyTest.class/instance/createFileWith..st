private
createFileWith: aName

	self directory ensureCreateDirectory.
	(OmFileStore fileReferenceForStoreNamed: aName inDirectory: self directory) 
		writeStreamDo: [ :stream | stream print: 'stub contents' ]