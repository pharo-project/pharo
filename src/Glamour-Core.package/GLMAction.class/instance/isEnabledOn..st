testing
isEnabledOn: aPresentation
	^ self enabledCondition glamourValueWithArgs: 
			(Array 
				with: aPresentation 
				withAll: aPresentation entity asGlamorousArray)