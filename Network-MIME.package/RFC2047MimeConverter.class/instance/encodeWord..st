private-encoding
encodeWord: aString

	(aString allSatisfy: [:c | c asciiValue < 128])
		ifTrue: [^ aString].
	^ String streamContents: [:stream |
		stream nextPutAll: '=?iso-8859-1?Q?'.
		aString do: [:c | self encodeChar: c to: stream].
		stream nextPutAll: '?=']