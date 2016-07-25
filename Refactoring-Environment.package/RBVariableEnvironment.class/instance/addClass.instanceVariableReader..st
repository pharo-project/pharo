accessing
addClass: aClass instanceVariableReader: aString 
	(instanceVariableReaders at: aClass name ifAbsentPut: [Set new]) 
		add: aString.
	self flushCachesFor: aClass.
	self addSearchString: aString