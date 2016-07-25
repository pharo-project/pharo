matching
matchAgainst: aMatcher
	"If the rest of the link chain matches successfully, report the
	position of the stream *before* the match started to the matcher."

	| startPosition |
	startPosition := aMatcher position.
	(next matchAgainst: aMatcher)
		ifTrue:
			[aMatcher markerPositionAt: index add: startPosition.
			^true].
	^false