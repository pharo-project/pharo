accessing
removeClass: aClass instanceVariableWriter: aString 
	| vars |
	vars := instanceVariableWriters at: aClass name ifAbsent: [Set new].
	vars remove: aString ifAbsent: [].
	vars isEmpty 
		ifTrue: [instanceVariableWriters removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass