operations
commentSelection

	| newCommentOrNil oldComment entryReferences |
	oldComment := self oldComment.
	entryReferences := self selectedEntryItems collect: #reference.
	
	newCommentOrNil := UIManager default
		multiLineRequest: 'Enter a comment for the selection' 
		initialAnswer: oldComment
		answerHeight: 250.

	newCommentOrNil ifNil: [ ^ self ].
	
	self logOffEvent:
		(EpLogEntriesComment 
			entryReferences: entryReferences 
			oldComment: oldComment 
			newComment: newCommentOrNil)