removing
removeClass: aClass into: aValue 
	(environmentDictionaries at: aValue ifAbsent: [RBSelectorEnvironment new]) 
		removeClass: aClass