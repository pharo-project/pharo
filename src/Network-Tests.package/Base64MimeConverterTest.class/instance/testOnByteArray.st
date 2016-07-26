tests
testOnByteArray
	"self run: #testOnByteArray"
	
	self assert: ('Hi There!' base64Encoded = 'Hi There!' asByteArray base64Encoded)