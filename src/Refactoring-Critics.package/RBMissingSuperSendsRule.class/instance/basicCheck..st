running
basicCheck: aMethod 
	| definer superMethod |
	(aMethod methodClass isMeta not and: [ self superMessages includes: aMethod selector ]) ifTrue: 
		[ definer := aMethod methodClass superclass ifNotNil: [ :sc | sc whichClassIncludesSelector: aMethod selector ].
		definer ifNotNil: 
			[ "super defines same method"
			(aMethod superMessages includes: aMethod selector) ifFalse: 
				[ "but I don't call it"
				superMethod := definer 
					compiledMethodAt: aMethod selector
					ifAbsent: [  ].
				^ (superMethod isReturnSelf or: [ superMethod sendsSelector: #subclassResponsibility ]) not ] ]].
	
	^ false