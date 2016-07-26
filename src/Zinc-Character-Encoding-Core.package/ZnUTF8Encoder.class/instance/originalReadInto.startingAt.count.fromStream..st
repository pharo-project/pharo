private
originalReadInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd.
	I signal a ZnByteStringBecameWideString notification if necessary"

	| stringBuffer |
	stringBuffer := string.
	0 to: requestedCount - 1 do: [ :count |
		| char |
		stream atEnd ifTrue: [ ^ count ].
		char := self nextFromStream: stream.
		(char isOctetCharacter not and: [ stringBuffer isWideString not ])
			ifTrue: [ stringBuffer := ZnByteStringBecameWideString convert: stringBuffer ].
		stringBuffer at: offset + count put: char ].
	^ requestedCount