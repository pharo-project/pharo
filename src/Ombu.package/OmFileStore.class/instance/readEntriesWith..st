private
readEntriesWith: aBlockClosure

	self fileReference readStreamDo: [ :readStream | [
		^ aBlockClosure value: readStream ascii ]
			on: Error
			do: [ :error |
				(OmFileStoreReadingError 
					readingError: error
					on: self fileReference
					position: readStream position) signal ] ]