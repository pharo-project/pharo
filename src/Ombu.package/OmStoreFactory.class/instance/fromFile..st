accessing
fromFile: aFileReference

	^ self 
		named: (OmFileStore globalNameFrom: aFileReference)
		inDirectory: aFileReference parent