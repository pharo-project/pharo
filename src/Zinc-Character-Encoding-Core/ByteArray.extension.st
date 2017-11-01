Extension { #name : #ByteArray }

{ #category : #'*Zinc-Character-Encoding-Core' }
ByteArray >> decodeWith: encoding [
	"Produce a String that decodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] decodeWith: #utf8"
	
	^ encoding asZnCharacterEncoder decodeBytes: self
]

{ #category : #'*Zinc-Character-Encoding-Core' }
ByteArray >> utf8Decoded [
	"Produce a String decoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."

	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] utf8Decoded"
	
	^ self decodeWith: ZnCharacterEncoder utf8
]
