tests
testCommentExportWithDependencyFix

	| selection oldComment newComment |
	oldComment := nil.
	selection := inputLog2 entries.
	
	inputLog2 store entriesDo: [ :entry |
		entry content isEpLogEntriesComment ifTrue: [
			oldComment := entry ]
		].
	
	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: selection.
		
	self assert: outputLog entries size equals: selection size.
	
	outputLog store entriesDo: [ :entry |
		entry content isEpLogEntriesComment ifTrue: [
			newComment := entry ]
		].

	self deny: ((oldComment tagAt: EpLog priorReferenceKey) = (newComment tagAt: EpLog priorReferenceKey)).
	self deny: ((oldComment tagAt: OmStore selfReferenceKey) = (newComment tagAt: OmStore selfReferenceKey)).
	self deny: (oldComment content entryReferences = newComment content entryReferences)
	