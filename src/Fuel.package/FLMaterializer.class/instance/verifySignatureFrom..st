protected
verifySignatureFrom: aDecoder

	| streamSignature |
	streamSignature := ByteArray new: self signature size.
	aDecoder nextEncodedBytesInto: streamSignature.
	(self signature asByteArray = streamSignature) ifFalse: [ 
		FLBadSignature 
			signalCurrentSignature: self signature 
			streamSignature: streamSignature  ].
