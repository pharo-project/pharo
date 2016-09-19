cleanup
rebuildSelectorTable
	self initSelectorTable.
	CompiledMethod
		allInstancesDo: [ :method | 
			| selector |
			selector := method selector.
			selector ifNotNil: [ SelectorTable add: selector ] ]