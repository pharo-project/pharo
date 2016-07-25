accessing
asSelectorEnvironment

	^RBSelectorEnvironment new
		searchStrings:#();
		label:self label;
		onEnvironment: self environment;
		classSelectors: self classSelectorDictionary metaClassSelectors: self metaClassSelectorDictionary;
		yourself.