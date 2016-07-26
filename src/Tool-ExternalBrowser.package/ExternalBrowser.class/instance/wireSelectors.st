initialize
wireSelectors
	selectors
		whenSelectedItemChanged: [ :selector | 
			selector 
				ifNil: [ self resetMethodBodyPane ] 
				ifNotNil: [ method text:  (self methodFor: selector) ]]