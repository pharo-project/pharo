accessing
displayValue
	
	^ columnToSortBy 
		ifNil: [self glamourPresentation displayValue]
		ifNotNil: [self glamourPresentation displayValue sorted: self sortBlock]