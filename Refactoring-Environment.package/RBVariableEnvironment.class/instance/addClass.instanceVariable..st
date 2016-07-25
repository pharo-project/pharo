accessing
addClass: aClass instanceVariable: aString 
	(instanceVariables at: aClass name ifAbsentPut: [Set new]) add: aString.
	self flushCachesFor: aClass.
	self addSearchString: aString