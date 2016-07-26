private
scanForEmptySlotFor: aKey
	"Scan the key array for the first slot containing an empty slot (indicated by flag or a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := aKey hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == flag or: [ element == nil ]) ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace