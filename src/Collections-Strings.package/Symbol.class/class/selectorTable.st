private
selectorTable
	^SelectorTable ifNil: [self initSelectorTable. SelectorTable]