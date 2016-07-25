splitjoin
split: aSequenceableCollection indicesDo: aBlock
	
	"Split a collection by myself as a delimitier.
	see Object >> split: for optimized version for single delimiters.
	Example:
		'||' split: 'foo||bar||2'"
	
	| position oldPosition |
	
	position := 1.
	oldPosition := position.
	position := aSequenceableCollection indexOfSubCollection: self startingAt: position.
	
	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + self size.
		oldPosition := position.
		position := aSequenceableCollection indexOfSubCollection: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size.

