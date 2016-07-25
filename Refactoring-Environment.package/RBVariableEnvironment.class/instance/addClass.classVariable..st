accessing
addClass: aClass classVariable: aSymbol 
	(classVariables at: aClass name ifAbsentPut: [Set new]) add: aSymbol.
	self flushCachesFor: aClass.
	self addSearchString: aSymbol