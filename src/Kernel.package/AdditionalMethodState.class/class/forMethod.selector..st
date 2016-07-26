instance creation
forMethod: aMethod selector: aSelector
	^(self new: 0)
		selector: aSelector;
		setMethod: aMethod;
		yourself