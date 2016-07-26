*Multilingual-TextConversion
convertToEncoding: encodingName
	^self convertToWithConverter: (TextConverter newForEncoding: encodingName).