running
checkMethod: aMethod
	| message |
	aMethod methodClass isTrait ifTrue: [ ^self].
	message := aMethod superMessages
		detect: [ :each | aMethod methodClass superclass isNil or: [ (aMethod methodClass superclass canUnderstand: each) not ] ]
		ifNone: [ aMethod selfMessages detect: [ :each | (aMethod methodClass allSelectors includes: each) not ] ifNone: [ nil ] ].
	message ifNotNil: [ result
				addSearchString: message;
				addMethod: aMethod ]