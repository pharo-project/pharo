queries
supportsNonASCII

	^ (Smalltalk globals includesKey: #Unicode) and: [ 
		((Smalltalk globals at: #EncodedCharSet) charsetAt: 255) name = #Unicode ]