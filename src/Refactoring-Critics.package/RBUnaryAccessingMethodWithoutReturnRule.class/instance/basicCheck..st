running
basicCheck: aMethod
	(aMethod numArgs > 0 or: [ aMethod isAbstract ])
		ifTrue: [ ^ false ].
	((aMethod methodClass organization categoryOfElement: aMethod selector) asString
		beginsWith: 'accessing')
		ifFalse: [ ^ false ].
	aMethod parseTree
		nodesDo: [ :each | 
			each isReturn
				ifTrue: [ ^ false ] ].
	^ true