adding
addClass: aClass selector: aSymbol into: aValue 
	(environmentDictionaries at: aValue ifAbsentPut: [RBSelectorEnvironment new]) 
		addClass: aClass
		selector: aSymbol