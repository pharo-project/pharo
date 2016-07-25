conversion
unicodeToByte: unicodeChar

	^unicodeChar charCode < 256
		ifTrue: [unicodeChar]
		ifFalse: [0 asCharacter]