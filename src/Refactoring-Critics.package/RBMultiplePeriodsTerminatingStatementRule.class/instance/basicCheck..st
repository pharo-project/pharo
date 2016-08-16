running
basicCheck: aMethod
	aMethod parseTree
		nodesDo: [ :each | 
			(each isSequence and: [ each periods size > each statements size ])
				ifTrue: [ ^ true ] ].
	^ false