accessing
removeClass: aClass instanceVariable: aString 
	| vars |
	vars := instanceVariables at: aClass name ifAbsent: [Set new].
	vars remove: aString ifAbsent: [].
	vars isEmpty 
		ifTrue: [instanceVariables removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass