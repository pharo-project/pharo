private
ombuStore

	| fileReference |
	fileReference := self fileReference.
	
	^ (OmFileStore 
		named: (OmFileStore globalNameFrom: fileReference) 
		inDirectory: fileReference parent)
		yourself