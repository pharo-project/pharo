menu - operations
commentSelection

	| items newCommentOrNil oldComment entryReferences |
	items := self selectedEntryItems.
	entryReferences := items collect: #reference.
	oldComment := items anyOne oldComment.
		
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