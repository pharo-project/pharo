testing
isVisible
	
	^ self methodOfInteresetSelectors anySatisfy: [ :selector |
		self perform: selector ]