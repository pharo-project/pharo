conversion
unicodeToByte: unicodeChar

	^unicodeChar charCode < 128
		ifTrue: [unicodeChar]
		ifFalse: [self class unicodeToByteTable at: unicodeChar charCode ifAbsent: [0 asCharacter]]