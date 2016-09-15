accessing
selectedCodeChanges

	^ self selectedEntryItems flatCollect: [ :item | 
		item entries select: [ :each | each content isCodeChange ] ]