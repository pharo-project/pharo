*Network-MIME
base64Decoded
	"Decode the receiver from base 64"
	"'SGVsbG8gV29ybGQ=' base64Decoded"
	^(Base64MimeConverter mimeDecode: self as: self class)