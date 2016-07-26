lookup
findVariable: lookupBlock ifNone: notFoundBlock 

	(lookupBlock value: selfVar) ifTrue: [ ^ selfVar ].
	(lookupBlock value: superVar) ifTrue: [ ^ superVar ].
	
	vars valuesDo: [ :var | (lookupBlock value: var) ifTrue: [ ^ var ] ].

	^ outerScope findVariable: lookupBlock ifNone: notFoundBlock 