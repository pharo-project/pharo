Class {
	#name : 'FileAPITest',
	#superclass : 'TestCase',
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'tests' }
FileAPITest >> testHelloWorld [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := FileAPI openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	file close.
	
	file := FileAPI openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	FileAPI deleteFile: path.
]

{ #category : 'tests' }
FileAPITest >> testHelloWorld2 [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := FileAPI openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	
	buffer := ByteArray new: file size.
	file position: 0.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	FileAPI deleteFile: path.
]

{ #category : 'tests' }
FileAPITest >> testHelloWorldAbsolutePath [
	| path text file buffer decodedText |
	path := 'test.txt' asFileReference asAbsolute fullName.
	text := 'Hello World'.
	
	file := FileAPI openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	file close.
	
	file := FileAPI openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	FileAPI deleteFile: path.
]

{ #category : 'tests' }
FileAPITest >> testHelloWorldAtFileOffset [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := FileAPI openCreateAlways: path.
	file writeFrom: text utf8Encoded at: 0.
	file close.
	
	file := FileAPI openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer at: 0.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	FileAPI deleteFile: path.
]

{ #category : 'tests' }
FileAPITest >> testHelloWorldAtFileOffset2 [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := FileAPI openCreateAlways: path.
	file writeFrom: text utf8Encoded at: 0.

	buffer := ByteArray new: file size.
	file readInto: buffer at: 0.

	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	FileAPI deleteFile: path.
]

{ #category : 'tests' }
FileAPITest >> testHelloWorldContents [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := FileAPI openCreateAlways: path.
	file writeFrom: text utf8Encoded.

	buffer := file contents.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	FileAPI deleteFile: path.
]

{ #category : 'tests' }
FileAPITest >> testTruncate [
	| path file |
	path := 'test.bin'.
	
	file := FileAPI openCreateAlways: path.
	file truncateToSize: 4096.
	self assert: file size equals: 4096.
	file close.
	
	FileAPI deleteFile: path.
]
