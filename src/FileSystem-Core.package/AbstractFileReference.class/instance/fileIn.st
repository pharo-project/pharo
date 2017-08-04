ToDeprecate
fileIn

	self readStreamDo: [ :stream | 
		CodeImporter evaluateFileStream: stream ]