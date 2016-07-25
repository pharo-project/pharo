accessing
postpone: aCollectionOfEvents
	| lastExitIndex |
	lastExitIndex := aCollectionOfEvents findLast: [ :each | each isCloseSpotter ].
	lastExitIndex = aCollectionOfEvents size ifTrue: [ 
		postponedEvents := nil.
		^ aCollectionOfEvents ].
	postponedEvents := aCollectionOfEvents copyFrom: lastExitIndex + 1 to: aCollectionOfEvents size.
	^ aCollectionOfEvents first: lastExitIndex.