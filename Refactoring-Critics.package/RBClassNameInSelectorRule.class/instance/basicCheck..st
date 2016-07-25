running
basicCheck: aMethod 
	(aMethod methodClass isMeta) ifFalse: [ ^ false ].
	^ (aMethod selector 
		indexOfSubCollection: aMethod methodClass soleInstance name
		startingAt: 1) > 0