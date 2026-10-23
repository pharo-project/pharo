Class {
	#name : 'OldFileTest',
	#superclass : 'TestCase',
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'tests' }
OldFileTest >> testHelloWorld [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := OldFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	file close.
	
	file := OldFile openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	OldFile deleteFile: path.
]

{ #category : 'tests' }
OldFileTest >> testHelloWorld2 [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := OldFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	
	buffer := ByteArray new: file size.
	file position: 0.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	OldFile deleteFile: path.
]

{ #category : 'tests' }
OldFileTest >> testHelloWorldAbsolutePath [
	| path text file buffer decodedText |
	path := 'test.txt' asFileReference asAbsolute fullName.
	text := 'Hello World'.
	
	file := OldFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.
	file close.
	
	file := OldFile openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	OldFile deleteFile: path.
]

{ #category : 'tests' }
OldFileTest >> testHelloWorldAtFileOffset [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := OldFile openCreateAlways: path.
	file writeFrom: text utf8Encoded at: 0.
	file close.
	
	file := OldFile openReadOnly: path.
	buffer := ByteArray new: file size.
	file readInto: buffer at: 0.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	OldFile deleteFile: path.
]

{ #category : 'tests' }
OldFileTest >> testHelloWorldAtFileOffset2 [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := OldFile openCreateAlways: path.
	file writeFrom: text utf8Encoded at: 0.

	buffer := ByteArray new: file size.
	file readInto: buffer at: 0.

	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.

	OldFile deleteFile: path.
]

{ #category : 'tests' }
OldFileTest >> testHelloWorldContents [
	| path text file buffer decodedText |
	path := 'test.txt'.
	text := 'Hello World'.
	
	file := OldFile openCreateAlways: path.
	file writeFrom: text utf8Encoded.

	buffer := file contents.
	file close.
	
	decodedText := buffer utf8Decoded.
	self assert: decodedText equals: text.
	
	OldFile deleteFile: path.
]

{ #category : 'tests' }
OldFileTest >> testTruncate [
	| path file |
	path := 'test.bin'.
	
	file := OldFile openCreateAlways: path.
	file truncateToSize: 4096.
	self assert: file size equals: 4096.
	file close.
	
	OldFile deleteFile: path.
]
