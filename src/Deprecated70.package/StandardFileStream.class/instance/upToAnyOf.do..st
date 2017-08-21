accessing
upToAnyOf: delimiters do: aBlock
	"Fast version to speed up nextChunk"
	| pos buffer count result |
	collection ifNotNil: [
		(position < readLimit and: [
			(count := collection indexOfAnyOf: delimiters startingAt: position + 1) <= readLimit and: [
				count > 0 ] ]) ifTrue: [
					result := collection copyFrom: position + 1 to: (position := count) - 1.
					aBlock value: (collection at: count).
					^result ] ].
	pos := self position.
	buffer := self next: 2000.
	(count := buffer indexOfAnyOf: delimiters) > 0 ifTrue: 
		["Found one of the delimiters part way into buffer"
		self position: pos + count.
		aBlock value: (buffer at: count).
		^ buffer copyFrom: 1 to: count - 1].
	self atEnd ifTrue:
		["Never found it, and hit end of file"
		^ buffer].
	"Never found it, but there's more..."
	^ buffer , (self upToAnyOf: delimiters do: aBlock)