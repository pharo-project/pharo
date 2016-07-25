requests
selectVariableToMoveMethodTo: aSelector class: aClass 
	^(self options at: #selectVariableToMoveTo) 
		value: self
		value: aClass
		value: aSelector