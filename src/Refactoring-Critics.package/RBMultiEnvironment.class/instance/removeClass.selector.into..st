removing
removeClass: aClass selector: aSelector into: aValue 
	(environmentDictionaries at: aValue ifAbsentPut: [RBSelectorEnvironment new]) 
		removeClass: aClass
		selector: aSelector