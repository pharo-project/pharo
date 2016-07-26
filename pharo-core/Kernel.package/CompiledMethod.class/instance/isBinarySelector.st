testing
isBinarySelector
	^self selector
		allSatisfy: [:each | each isSpecial]