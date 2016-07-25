testing
testWritingOverflow
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedWriteStream on: stringStream.
		bufferedStream sizeBuffer: 8.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream nextPutAll: '0123'; nextPutAll: '4567'; nextPutAll: '89'.
		bufferedStream nextPutAll: '0123456789'; nextPutAll: '0123456789'.
		bufferedStream flush ].
	self assert: string = '0123456789012345678901234567890123456789'