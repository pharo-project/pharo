adding/removing methods
addSelector: selector withMethod: compiledMethod 
	^ self addSelector: selector withMethod: compiledMethod notifying: nil