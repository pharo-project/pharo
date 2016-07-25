testing
testWriting
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedWriteStream on: stringStream.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream flush ].
	self assert: string = '0123456789'