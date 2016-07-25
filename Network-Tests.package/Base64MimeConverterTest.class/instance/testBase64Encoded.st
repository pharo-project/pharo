tests
testBase64Encoded
	"self run: #testBase64Encoded"
	
	| encoded |
	encoded := (Base64MimeConverter mimeEncode: message) contents.
	self assert: encoded = 'Hi There!' base64Encoded.
	