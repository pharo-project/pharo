matching
matchSelectorAgainst: aNode inContext: aDictionary 
	self selectorParts with: aNode selectorParts do: [ :first :second |
		| keyword |
		keyword := aDictionary
			at: first
			ifAbsentPut: [ 
				first isPatternVariable 
					ifTrue: [ second ]
					ifFalse: [ first ] ].
		keyword = second 
			ifFalse: [ ^ false ] ].
	^ true