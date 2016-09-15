private
error: sourceError reading: aFileReference position: aFilePosition
		
	^ (OmFileStoreReadingError readingError: sourceError on: aFileReference position: aFilePosition) signal