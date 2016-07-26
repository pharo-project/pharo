actions
merge
	merger isMerged
		ifFalse: [self inform: 'You must resolve all the conflicts first']
		ifTrue: [self answer: true] 