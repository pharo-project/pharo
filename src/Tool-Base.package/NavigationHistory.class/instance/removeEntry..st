protocol
removeEntry: anEntry
	| toRemove |
	
	"we know that toRemove will always contains one element"
	toRemove := (storage select: [:e | e key == anEntry ]).
	toRemove ifEmpty: [ ^ self ].
	toRemove := toRemove first.
	storage remove: toRemove.
	storage addLast: toRemove