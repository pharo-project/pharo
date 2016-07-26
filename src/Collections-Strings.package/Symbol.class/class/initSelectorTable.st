private
initSelectorTable
	SelectorTable := WeakSet new.
	CompiledMethod
		allInstancesDo: [ :method | 
			| selector |
			selector := method selector.
			selector ifNotNil: [ SelectorTable add: selector ] ]