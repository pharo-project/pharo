*Collections-Abstract-splitjoin
split: aSequenceableCollection indicesDo: aBlock
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar' indicesDo: [ :start :end | ]"
		
	|  position oldPosition |
	
	position := 1.
	oldPosition := position.
	
	position := aSequenceableCollection indexOf: self startingAt: position.
	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + 1.
		oldPosition := position.
		position := aSequenceableCollection indexOf: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size.
