Extension { #name : #Base64MimeConverter }

{ #category : #'*Deprecated80' }
Base64MimeConverter class >> mimeDecodeToBytes: aStream [ 

	self deprecated: 'Please use #base64Decoded now. See issue #21937'.
	
	^aStream contents base64Decoded readStream 
]
