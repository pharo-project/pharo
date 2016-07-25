accessing
classVariableNames: aCollectionOfStrings 
	classVariableNames := (aCollectionOfStrings 
				collect: [:each | each asSymbol]) asOrderedCollection