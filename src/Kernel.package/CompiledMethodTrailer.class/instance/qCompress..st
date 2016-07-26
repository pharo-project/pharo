private
qCompress: string
	"A very simple text compression routine designed for method temp names.
	 Most common 11 chars get values 1-11 packed in one 4-bit nibble;
	 the next most common get values 12-15 (2 bits) * 16 plus next nibble;
	 unusual ones get three nibbles, the first being the escape nibble 0.

	Answer the write stream with compressed data inside"

	| utf8str stream oddNibble |

	string isEmpty ifTrue:
		[^self qCompress: ' '].
	utf8str := string convertToEncoding: 'utf8'.

	stream := WriteStream on: (ByteArray new: utf8str size).
	oddNibble := nil.

	utf8str do:	[:char | | ix |
		ix := 'ear tonsilcmbdfghjkpquvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345[]()'
			indexOf: char ifAbsent: 0.
		(ix = 0
			ifTrue:
				[{ 0. char asInteger // 16. char asInteger \\ 16 }]
			ifFalse:
				[ix <= 11
					ifTrue: [{ ix }]
					ifFalse: [{ ix//16+12. ix\\16 }]])
				do: [:nibble |
					oddNibble
						ifNotNil: [stream nextPut: oddNibble*16 + nibble. oddNibble := nil]
						ifNil: [oddNibble := nibble]]].
	oddNibble ifNotNil: "4 = 'ear tonsil' indexOf: Character space"
		[stream nextPut: oddNibble * 16 + 4].
	^ stream
