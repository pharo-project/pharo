instance creation
hasSuperclass: aClass 
	^self new 
		type: (Array with: #hasSuperclass with: aClass)
		block: [aClass superclass isNil not]
		errorString: aClass printString , ' has <1?a:no> superclass'