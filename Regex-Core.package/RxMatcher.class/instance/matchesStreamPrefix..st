accessing
matchesStreamPrefix: theStream
	"Match thyself against a positionable stream."

	stream := theStream.
	markerPositions := nil.
	^self tryMatch