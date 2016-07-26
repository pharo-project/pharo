testing
hasTempVector: aSymbol
	startSequence withAllSuccessorsDo: [ :seq | (seq hasTempVector: aSymbol) ifTrue: [ ^ true ]].
	^ false