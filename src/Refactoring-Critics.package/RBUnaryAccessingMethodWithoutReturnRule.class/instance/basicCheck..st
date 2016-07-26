running
basicCheck: aMethod
	aMethod numArgs = 0 ifFalse: [ ^ false ].
	aMethod isAbstract ifTrue: [ ^ false ].
	(aMethod methodClass organization categoryOfElement: aMethod selector) = #accessing
		ifFalse: [ ^ false ].
	^ aMethod parseTree statements noneSatisfy: [ :each | 
			each isReturn ].