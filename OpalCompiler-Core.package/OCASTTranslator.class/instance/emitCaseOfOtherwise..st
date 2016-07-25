inline messages
emitCaseOfOtherwise: aMessageNode

	self 
		emitCaseOf: aMessageNode 
		otherwiseBlock: [ valueTranslator visitInlinedBlockNode: aMessageNode arguments last ]