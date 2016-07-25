private
addStoreNamed: aGlobalName ifPossible: possibleBlockClosure ifNotPossible: notPossibleBlockClosure 

	| fileReference |
	fileReference := directoryFileReference / (aGlobalName, OmFileStore defaultFileSuffix).
	
	^ fileReference exists 
		ifFalse: notPossibleBlockClosure
		ifTrue: [ 
			| aStore |
			aStore := (OmFileStore 
				named: aGlobalName
				inDirectory: directoryFileReference)
				refresh;
				yourself.
				
			compositeStore addStore: aStore.
			
			possibleBlockClosure value: aStore ]