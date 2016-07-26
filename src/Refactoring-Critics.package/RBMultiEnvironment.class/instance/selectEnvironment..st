accessing
selectEnvironment: aValue 
	environment := environmentDictionaries at: aValue
				ifAbsent: [RBSelectorEnvironment new]