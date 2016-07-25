removing
removeClass: aClass 
	aClass isMeta
		ifTrue: [metaClasses remove: aClass soleInstance name ifAbsent: []]
		ifFalse: [classes remove: aClass name ifAbsent: []]