removing
removeClass: aClass selector: aSelector 
	(aClass isMeta 
		ifTrue: [metaClassSelectors at: aClass soleInstance name ifAbsent: [^self]]
		ifFalse: [classSelectors at: aClass name ifAbsent: [^self]]) 
			remove: aSelector
			ifAbsent: []