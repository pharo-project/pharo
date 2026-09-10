"
A NewFileTest is a test class for testing the behavior of NewFile
"
Class {
	#name : 'NewFileTest',
	#superclass : 'TestCase',
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'tests' }
NewFileTest >> testHelloWorld [
	| path text file buffer decodedText |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	file close.
	
	file := NewFile openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testHelloWorld2 [
	| path text file buffer decodedText |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	
	buffer := ByteArray new: file size.
	file position: 0.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testHelloWorldAbsolutePath [
	| path text file buffer decodedText |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt' asFileReference asAbsolute fullName.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	file close.
	
	file := NewFile openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testHelloWorldAtFileOffset [
	| path text file buffer decodedText |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded at: 0.
	file close.
	
	file := NewFile openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer at: 0.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testHelloWorldAtFileOffset2 [
	| path text file buffer decodedText |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded at: 0.

	buffer := ByteArray new: file size.
	file readInto: buffer at: 0.

	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testHelloWorldContents [
	| path text file buffer decodedText |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.

	buffer := file contents.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testHelloWorldReadOnlyMemoryMapping [
	| path text file buffer decodedText memoryMap |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := NewFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.

	memoryMap := file readOnlyMemoryMap.
	buffer := memoryMap contents.
	
	memoryMap unmap.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	NewFile deleteFile: path.
]

{ #category : 'tests' }
NewFileTest >> testTruncate [
	| path file |
	NewFile primitiveIsAvailable ifFalse: [ ^ self skip ].
	
	path := 'test.bin'.
	
	file := NewFile openCreateAlways: path.
	file truncateToSize: 4096.
	self assert: file size equals: 4096.
	file close.
	
	NewFile deleteFile: path.
]
