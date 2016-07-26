testing
testMultiPartFormDataWriteRead
	| input output bytes |
	input := (ZnMultiPartFormDataEntity new)
		addPart: (ZnMimePart fieldName: 'extra' value: 'my-extra');
		addPart: (ZnMimePart fieldName: 'info' value: 'my-info');
		addPart: (ZnMimePart fieldName: 'file' fileName: 'foo.txt' entity: (ZnEntity text: 'Zinc HTTP Components'));
		yourself.
	self assert: input contentLength notNil.
	self assert: input contentLength > 0.
	self assert: (input contentType matches: ZnMimeType multiPartFormData).
	bytes := ByteArray streamContents: [ :stream | input writeOn: stream ].
	output := (ZnMultiPartFormDataEntity type: input contentType) readFrom: bytes readStream.
	self assert: (output partNamed: 'extra') fieldValue = 'my-extra'.
	self assert: (output partNamed: 'info') fieldValue = 'my-info'.
	self assert: (output partNamed: 'extra') fieldValueString = 'my-extra'.
	self assert: (output partNamed: 'info') fieldValueString = 'my-info'.
	self assert: (output partNamed: 'file') contents = 'Zinc HTTP Components'.
	output := (ZnMultiPartFormDataEntity type: input contentType length: input contentLength) readFrom: bytes readStream.
	self assert: (output partNamed: 'extra') fieldValue = 'my-extra'.
	self assert: (output partNamed: 'info') fieldValue = 'my-info'.
	self assert: (output partNamed: 'extra') fieldValueString = 'my-extra'.
	self assert: (output partNamed: 'info') fieldValueString = 'my-info'.
	self assert: (output partNamed: 'file') contents = 'Zinc HTTP Components'.

	