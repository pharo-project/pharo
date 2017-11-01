Trait {
	#name : #MalTNormalize,
	#category : 'Moose-Algos-Graph'
}

{ #category : #running }
MalTNormalize >> normalize: aCollectionOfNodes [

	| firstNode perm |
	firstNode := aCollectionOfNodes detectMin: [:n| n printString ].
	perm := aCollectionOfNodes copy.
	[ firstNode = perm first ] whileFalse: [
		perm addLast: perm removeFirst ].
	^ perm
]
