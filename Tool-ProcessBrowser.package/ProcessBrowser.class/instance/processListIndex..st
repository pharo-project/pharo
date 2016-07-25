accessing
processListIndex: index 
	processListIndex := index.
	selectedProcess := processList
				at: index
				ifAbsent: [].
	self updateStackList.
	self changed: #processListIndex.