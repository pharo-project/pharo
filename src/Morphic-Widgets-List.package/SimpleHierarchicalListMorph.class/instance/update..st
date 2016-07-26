updating
update: aSymbol 
	aSymbol == getSelectionSelector 
		ifTrue: 
			[self selection: self getCurrentSelectionItem.
			^self].
	aSymbol == getListSelector 
		ifTrue: 
			[self list: self getList.
			^self].
	((aSymbol isKindOf: Array) 
		and: [aSymbol size > 1 and: [aSymbol first == getListSelector and: [
					aSymbol second == #openPath]]]) "allow directed path opening where multiple trees exist"
			ifTrue: 
				[^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openPath: (aSymbol allButFirst: 2)].
	((aSymbol isKindOf: Array) 
		and: [aSymbol size > 1 and: [aSymbol first == getListSelector and: [
					aSymbol second == #openItemPath]]]) "allow directed path opening where multiple trees exist"
			ifTrue: 
				[^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openItemPath: (aSymbol allButFirst: 2)].
	((aSymbol isKindOf: Array) 
		and: [aSymbol notEmpty and: [aSymbol first == #openPath]]) 
			ifTrue: 
				[^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openPath: aSymbol allButFirst].
	((aSymbol isKindOf: Array) 
		and: [aSymbol size  = 2 and: [aSymbol first = getListSelector and: [
					aSymbol second == #expandRoots]]]) 
			ifTrue: 
				[^self expandRoots].
	((aSymbol isKindOf: Array) 
		and: [aSymbol notEmpty and: [aSymbol first = getListSelector and: [
					aSymbol second == #expandAll]]]) 
			ifTrue: 
				[^self expandAll]