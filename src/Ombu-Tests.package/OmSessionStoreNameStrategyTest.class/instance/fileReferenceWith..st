private
fileReferenceWith: aName

	^ OmFileStore 
		fileReferenceForStoreNamed: aName
		inDirectory: self directory