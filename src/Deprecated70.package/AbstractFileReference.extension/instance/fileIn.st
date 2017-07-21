*Deprecated70
fileIn

	self readStreamDo: [ :stream | 
		CodeImporter evaluateFileStream: stream ]