Extension { #name : #String }

{ #category : #'*Zinc-Character-Encoding-Core' }
String >> asZnCharacterEncoder [
	"Return a ZnCharacterEncoder instance using the receiver as identifier"
	
	" 'UTF-8' asZnCharacterEncoder "
	
	^ ZnCharacterEncoder newForEncoding: self
]

{ #category : #'*Zinc-Character-Encoding-Core' }
String >> encodeWith: encoding [
	"Produce a ByteArray that encodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	" 'Les élèves français' encodeWith: #utf8 "
	
	^ encoding asZnCharacterEncoder encodeString: self
]

{ #category : #'*Zinc-Character-Encoding-Core' }
String >> urlDecoded [
	"URL Decode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new decode: self
]

{ #category : #'*Zinc-Character-Encoding-Core' }
String >> urlEncoded [
	"URL Encode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new encode: self

]

{ #category : #'*Zinc-Character-Encoding-Core' }
String >> utf8Encoded [
	"Produce a ByteArray encoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."
	
	" 'Les élèves français' utf8Encoded "
	
	^ self encodeWith: ZnCharacterEncoder utf8
]
