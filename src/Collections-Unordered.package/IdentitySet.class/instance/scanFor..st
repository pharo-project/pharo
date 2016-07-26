private
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start hash |
	hash := anObject identityHash.
	index := start := hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == nil or: [ element enclosedSetElement == anObject ])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace