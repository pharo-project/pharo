accessing
nextTo: aName in: aDirectory
	
	| fileReference |
	fileReference := OmFileStore
		fileReferenceForStoreNamed: aName
		inDirectory: aDirectory.
	
	^ fileReference nextVersion basenameWithoutExtension
