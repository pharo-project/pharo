menu - accessing
selectedCodeChanges

	^ self selectedEntryItems 
		select: [ :each | each entry content isCodeChange ]
		thenCollect: [ :each | each entry ]