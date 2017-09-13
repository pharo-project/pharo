running
check: aMethod forCritiquesDo: aCriticBlock

	(aMethod numArgs > 0 or: [ aMethod isAbstract ]) ifTrue: [ ^ self ].
	((aMethod methodClass organization categoryOfElement: aMethod selector) asString
		beginsWith: 'accessing')
		ifFalse: [ ^ self ].
	aMethod parseTree
		nodesDo: [ :each | 
			each isReturn
				ifTrue: [ ^ self ] ].
	
	aCriticBlock cull:  (self critiqueFor: aMethod)