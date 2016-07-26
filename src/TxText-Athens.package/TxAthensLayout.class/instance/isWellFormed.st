testing
isWellFormed
	"check that every position within the receiver interval corresponds to some line "

	| pos end |

	pos := self startPosition.
	end := self endPosition.
	
	[ pos <= end ] whileTrue: [ 
		self positionAt: pos ifOutside: [ ^ false ].
		pos := pos nextPosition.
	 ].
	^ true.