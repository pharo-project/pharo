materializing
materializeHeaderFrom: aStream

	| aDecoder |
	aDecoder := 
		FLDecoder 
			on: aStream 
			migrations: migrations 
			globalEnvironment: globalEnvironment.
	self verifySignatureFrom: aDecoder.
	self verifyVersionFrom: aDecoder.
	^ self decodeHeaderWith: aDecoder