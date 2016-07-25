accessing
next: bytesCount putAll: aCollection startingAt: startIndex
	| start count max |
	aCollection species = collection species
		ifFalse:[
			aCollection do:[:ch| self nextPut: ch].
			^ aCollection].
	start := startIndex.
	count := bytesCount.
	[count = 0] whileFalse:[
		position = writeLimit ifTrue:[self deflateBlock].
		max := writeLimit - position.
		max > count ifTrue:[max := count].
		collection replaceFrom: position+1
			to: position+max
			with: aCollection
			startingAt: start.
		start := start + max.
		count := count - max.
		position := position + max].
	^ aCollection