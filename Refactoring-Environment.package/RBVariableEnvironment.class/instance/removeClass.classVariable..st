accessing
removeClass: aClass classVariable: aSymbol 
	| vars |
	vars := classVariables at: aClass name ifAbsent: [Set new].
	vars remove: aSymbol ifAbsent: [].
	vars isEmpty ifTrue: [classVariables removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass