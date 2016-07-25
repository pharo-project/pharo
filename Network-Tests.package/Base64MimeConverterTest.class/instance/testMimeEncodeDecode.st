tests
testMimeEncodeDecode
	"self run: #testMimeEncodeDecode"
	
	| encoded |
	encoded := Base64MimeConverter mimeEncode: message.
	self assert: (encoded contents = 'SGkgVGhlcmUh').
      self assert: ((Base64MimeConverter mimeDecodeToChars: encoded) contents = message contents).

	"Encoding should proceed from the current stream position."
	message reset.
	message skip: 2.
	encoded := Base64MimeConverter mimeEncode: message.
	self assert: (encoded contents = 'IFRoZXJlIQ==').