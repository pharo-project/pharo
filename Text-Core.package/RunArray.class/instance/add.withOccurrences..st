adding
add: newObject withOccurrences: anInteger
	"Add value as the last element of the receiver, the given number of times"
	anInteger <= 0 ifTrue: [ ^self ].
	lastIndex := nil. "flush access cache"
	(runs size=0 or: [values last ~= newObject])
		ifTrue:
			[runs := runs copyWith: anInteger.
			values := values copyWith: newObject]
		ifFalse:
			[runs at: runs size put: runs last + anInteger]