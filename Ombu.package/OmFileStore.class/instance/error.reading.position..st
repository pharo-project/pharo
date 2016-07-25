private
error: sourceError reading: aFileReference position: aFilePosition
		
	^ (OmFileStoreReadingError error: sourceError reading: aFileReference position: aFilePosition) signal