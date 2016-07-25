instance creation
referencesInstanceVariable: aString in: aClass 
	^self new 
		type: (Array with: #referencesInstVar with: aClass with: aString)
		block: [(aClass whichSelectorsReferToInstanceVariable: aString) isEmpty not]
		errorString: aClass printString 
				, ' <1?:does not >reference<1?s:> instance variable ' , aString