private
readingError: sourceError on: aFileReference position: aFilePosition
		
	^ (OmFileStoreReadingError readingError: sourceError on: aFileReference position: aFilePosition) signal