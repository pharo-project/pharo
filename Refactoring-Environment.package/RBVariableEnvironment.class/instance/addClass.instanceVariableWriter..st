accessing
addClass: aClass instanceVariableWriter: aString 
	(instanceVariableWriters at: aClass name ifAbsentPut: [Set new]) 
		add: aString.
	self flushCachesFor: aClass.
	self addSearchString: aString