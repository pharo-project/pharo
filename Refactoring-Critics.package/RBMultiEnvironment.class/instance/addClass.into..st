adding
addClass: aClass into: aValue 
	(environmentDictionaries at: aValue ifAbsentPut: [RBSelectorEnvironment new]) 
		addClass: aClass