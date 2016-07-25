pager-actions
pressedScrollerSizeCombination: aSymbol

	firstCombination ifNil: [ firstCombination := aSymbol. ^ self].
	
	self scrollerSizeShortcutAction: firstCombination second: aSymbol.
	firstCombination := nil.