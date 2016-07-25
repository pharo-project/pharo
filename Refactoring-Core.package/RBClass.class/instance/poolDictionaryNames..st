accessing
poolDictionaryNames: aCollectionOfStrings 
	poolDictionaryNames := (aCollectionOfStrings 
				collect: [:each | each asSymbol]) asOrderedCollection