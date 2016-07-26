running
basicCheck: aMethod 
	aMethod methodClass isMeta ifTrue: [ ^ false ].
	^ (matcher executeTree: aMethod ast initialAnswer: false) 
		