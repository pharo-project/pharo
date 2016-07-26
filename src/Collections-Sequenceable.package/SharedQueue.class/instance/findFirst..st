accessing
findFirst: aBlock
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no such object has been queued, answer <nil> and leave me intact."

	
	^monitor critical: [ | index |
		index := items findFirst: aBlock.
		index = 0 ifTrue: [
			nil ]
		ifFalse: [
			items removeAt: index ] ].
