deprecated
parseShortVersionString: aString 
	"Answer the parts of aString, expected in short format <type><major>.<minor><suffix>."

	| stream length type major minor suffix |

	self 
		deprecated: 'Use #fromVersionString: instead'
		on: '22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	
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