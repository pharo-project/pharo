running
checkMethod: aMethod 
	| message |
	(message := (aMethod superMessages)
		detect: [ :each | each ~= aMethod selector ]
		ifNone: [ nil ]) notNil ifTrue: 
		[ result
			addSearchString: message;
			addMethod: aMethod ] 