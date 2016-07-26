accessing
removeClass: aClass instanceVariableReader: aString 
	| vars |
	vars := instanceVariableReaders at: aClass name ifAbsent: [Set new].
	vars remove: aString ifAbsent: [].
	vars isEmpty 
		ifTrue: [instanceVariableReaders removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass