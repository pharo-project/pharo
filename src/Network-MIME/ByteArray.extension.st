Extension { #name : #ByteArray }

{ #category : #'*Network-MIME' }
ByteArray >> base64Encoded [
	"Encode the receiver using Base64, returning a String.
	Base64 encoding is a technique to represent binary data as ASCII text.
	The inverse operation is String>>#base64Decoded"
	
	"(0 to: 255) asByteArray base64Encoded"
	"(Integer primesUpTo: 255) asByteArray base64Encoded"
	"'Hello World!' utf8Encoded base64Encoded"
	
	^ ZnBase64Encoder new encode: self
]
