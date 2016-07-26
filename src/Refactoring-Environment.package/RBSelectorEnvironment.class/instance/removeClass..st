removing
removeClass: aClass 
	aClass isMeta 
		ifTrue: [metaClassSelectors removeKey: aClass soleInstance name ifAbsent: []]
		ifFalse: [classSelectors removeKey: aClass name ifAbsent: []]