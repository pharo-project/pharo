testing
isAvailableOn: aPresentation
	^ self condition glamourValueWithArgs: 
			(Array 
				with: aPresentation 
				withAll: aPresentation entity asGlamorousArray)