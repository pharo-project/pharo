private
objectClassSelectorFor: aTestSelector
	^ (aTestSelector asString allButFirst: 4) "'test' size"
		uncapitalized asSymbol