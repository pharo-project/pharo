requests
shouldUseExistingMethod: aSelector 
	^(self options at: #useExistingMethod) value: self value: aSelector