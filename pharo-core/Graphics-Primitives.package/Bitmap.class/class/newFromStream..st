instance creation
newFromStream: s 
	| len |
	s next = 128 ifTrue: 
		[ "New compressed format"
		len := self decodeIntFrom: s.
		^ Bitmap decompressFromByteArray: (s nextInto: (ByteArray new: len)) ].
	s skip: -1.
	len := s nextInt32.
	len <= 0 
		ifTrue: 
			[ "Old compressed format"
			^ (self new: len negated) readCompressedFrom: s ]
		ifFalse: 
			[ "Old raw data format"
			^ s nextWordsInto: (self new: len) ]