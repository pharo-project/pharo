accessing
matchesStream: theStream
	"Match thyself against a positionable stream."

	^(self matchesStreamPrefix: theStream)
		and: [stream atEnd]