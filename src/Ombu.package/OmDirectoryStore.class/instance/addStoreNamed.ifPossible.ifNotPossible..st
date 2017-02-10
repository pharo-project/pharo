private
addStoreNamed: aGlobalName ifPossible: possibleBlockClosure ifNotPossible: notPossibleBlockClosure 

	| fileReference |
	fileReference := OmFileStore fileReferenceForStoreNamed: aGlobalName inDirectory: directoryFileReference.
	
	^ fileReference exists 
		ifFalse: notPossibleBlockClosure
		ifTrue: [ 
			| aStore |
			aStore := (OmStoreFactory current
				named: aGlobalName
				inDirectory: directoryFileReference)
				refresh;
				yourself.
				
			compositeStore addStore: aStore.
			
			possibleBlockClosure value: aStore ]