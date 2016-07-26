*Network-Mime
base64Encoded
	"Encode the receiver as base64"
	"'Hello World' asByteArray base64Encoded"
	^(Base64MimeConverter mimeEncode: self readStream) contents