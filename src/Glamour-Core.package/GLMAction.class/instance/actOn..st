public interface
actOn: aPresentation 
	 
	^ self action 
		glamourValueWithArgs: 
			(Array 
				with: aPresentation 
				withAll: aPresentation entity asGlamorousArray)