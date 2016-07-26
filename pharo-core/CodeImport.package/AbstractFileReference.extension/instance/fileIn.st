*codeimport
fileIn

	self readStreamDo: [ :stream | 
		CodeImporter evaluateFileStream: stream ]