testing
testSetToEnd

	| string stream |
	string := 'hello'.
	stream := WriteStream with: ''.
	stream nextPutAll: string.
	self assert: stream position = string size.
	stream setToEnd.
	self assert: stream position = string size.
	self assert: stream contents = string