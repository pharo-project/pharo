private
scanForEmptySlotFor: aKey
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := (hashBlock
		ifNil: [ aKey hash ]
		ifNotNil: [ hashBlock value: aKey ]) \\ array size + 1.
	[ 
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace