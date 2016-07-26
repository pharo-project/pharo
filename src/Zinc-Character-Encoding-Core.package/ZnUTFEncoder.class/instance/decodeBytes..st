convenience
decodeBytes: bytes
	"Overridden to prevent the automagic switch from ByteString to WideString 
	and the slow #becomeForward: in there. See also #decodeBytesIntoWideString:"
	
	| byteStream |
	byteStream := bytes readStream.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [ | char |
			char := self nextFromStream: byteStream.
			(char isOctetCharacter not and: [ stream originalContents isWideString not ])
				ifTrue: [ | wideString position |
					position := stream position.
					wideString := WideString from: stream originalContents.
					stream on: wideString; setFrom: position + 1 to: position ].
			stream nextPut: char ] ] 