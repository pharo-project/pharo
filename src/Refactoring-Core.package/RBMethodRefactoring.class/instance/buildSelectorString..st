private
buildSelectorString: aSelector 
	aSelector numArgs = 0 ifTrue: [^aSelector].
	^self buildSelectorString: aSelector
		withPermuteMap: (1 to: aSelector numArgs)