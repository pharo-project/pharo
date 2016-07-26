conversion
mimeDecode
	"Do conversion reading from mimeStream writing to dataStream. See String>>decodeMimeHeader"

	| c |
	[mimeStream atEnd] whileFalse: [
		c := mimeStream next.
		c = $=
			ifTrue: [c := Character value: mimeStream next digitValue * 16
				+ mimeStream next digitValue]
			ifFalse: [c = $_ ifTrue: [c := $ ]].
		dataStream nextPut: c].
	^ dataStream