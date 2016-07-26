tests-Getting Started
testGZip

	| materialization |
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aFileStream | 
			|gzip|
			gzip := GZipWriteStream on: aFileStream.
			FLSerializer newDefault serialize: 123 on: gzip.
			gzip close ].
	
	FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [:aFileStream | 
			|gzip|
			gzip := GZipReadStream on: aFileStream.		
			materialization := FLMaterializer newDefault 
				materializeFrom: gzip ]