decoding
qDecompress 
	"Trailer is compressed string using qCompress method + length field + 1 byte
	
	Decompress strings compressed by qCompress:.
	Most common 11 chars get values 0-10 packed in one 4-bit nibble;
	next most common 52 get values 12-15 (2 bits) * 16 plus next nibble;
	escaped chars get three nibbles"
	| len str input |
	
	len := self decodeLengthField.
 	input := (ReadStream on: method from: method size - size+1 to: method size - size + len).
	
	str := String streamContents:
		[:strm | | nextNibble nibble peek charTable |
		charTable :=  "Character encoding table must match qCompress:"
		'ear tonsilcmbdfghjkpquvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345[]()'.
		peek := true.
		nextNibble := [peek
						ifTrue: [peek := false. input peek ifNil: [0] ifNotNil: [:b| b // 16]]
						ifFalse: [peek := true. input next ifNil: [0] ifNotNil: [:b| b \\ 16]]].
		[input atEnd] whileFalse:
			[(nibble := nextNibble value) = 0
				ifTrue: [input atEnd ifFalse:
						[strm nextPut: (Character value: nextNibble value * 16 + nextNibble value)]]
				ifFalse:
					[nibble <= 11
						ifTrue:
							[strm nextPut: (charTable at: nibble)]
						ifFalse:
							[strm nextPut: (charTable at: nibble-12 * 16 + nextNibble value)]]]].
			
	data := str convertFromEncoding: 'utf8'