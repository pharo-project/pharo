importing-methods
testImportFromReadStream
	self assert: 4 equals: (CodeImporter evaluateReadStream: '2+2!' readStream)