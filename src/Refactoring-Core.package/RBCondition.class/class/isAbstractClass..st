instance creation
isAbstractClass: aClass 
	^self new 
		type: (Array with: #IsAbstractClass with: aClass)
		block: [aClass isAbstract]
		errorString: aClass printString , ' is <1?:not >an abstract class'