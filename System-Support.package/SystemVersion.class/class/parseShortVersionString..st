accessing
parseShortVersionString: aString 
	"Answer the parts of aString, expected in short format <type><major>.<minor><suffix>."

	| stream length type major minor suffix |

	stream := aString readStream.
	[stream peek isDigit] whileFalse: [stream next].
	length := stream position.
	stream reset.
	type := (stream next: length).
	major := (stream upTo: $.) asInteger.
	stream := stream upToEnd readStream.
	[stream peek notNil and: [stream peek isDigit]] whileTrue: [stream next].
	length := stream position.
	stream reset.
	minor := (stream next: length) asInteger.
	suffix := stream upToEnd.
	^{type. major. minor. suffix}