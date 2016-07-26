display
selectedIndex: anIndex
	index := anIndex.
	self updateMethodSelection: (tasks at: index ifAbsent: [ ^self ]).