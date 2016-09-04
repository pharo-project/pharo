accessing
upTo: delim 
	"Fast version to speed up nextChunk"
	| pos buffer count |
	collection ifNotNil: [
		(position < readLimit and: [
			(pos := collection indexOf: delim startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^collection copyFrom: position + 1 to: (position := pos) - 1 ] ].
	pos := self position.
	buffer := self next: 2000.
	(count := buffer indexOf: delim) > 0 ifTrue: 
		["Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - 1].
	self atEnd ifTrue:
		["Never found it, and hit end of file"
		^ buffer].
	"Never found it, but there's more..."
	^ buffer , (self upTo: delim)