"
My instances provide an external file reference to a piece of text.  It may be the sourceCode of a method, or the class comments of a class.
I can return either a String or a Text.

In a source file (sources, changes, fileouts) we store chunks of code.
It is usually to store the source string of a method for example.

max: aNumber
	^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!
 
Some chunks are styled (they have parts in bold, or font color...).
When a chunk is styled, it is followed by a second chunk (prefixed with something like ]style[), and specifying the style.
The encoding is as follows:

max: aNumber
	^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!
]style[(14 50 312)f1,f1b,f1LInteger +;i!

Allowed TextAttributes are TextFontChange, TextEmphasis, TextColor, TextDoIt, TextKern, TextLink, TextURL. 
TextFontReference and TextAnchor are not supported.

See PositionableStream nextChunkText and RunArray class scanFrom:.
"
Class {
	#name : #RemoteString,
	#superclass : #Object,
	#instVars : [
		'sourceFileNumber',
		'filePositionHi',
		'sourceFileArray'
	],
	#category : #'System-Sources-Utilities'
}

{ #category : #'instance creation' }
RemoteString class >> newFileNumber: sourceIndex position: anInteger [ 
	"Answer an instance of me fora file indexed by sourceIndex, at the 
	position anInteger. Assume that the string is already stored on the file 
	and the instance will be used to access it."

	^self new fileNumber: sourceIndex position: anInteger
]

{ #category : #'instance creation' }
RemoteString class >> newString: aString [
	"Answer an instance of me for string, aString, on changes file. 
	Put the string on the file and create the remote reference."

	^self new string: aString
]

{ #category : #'instance creation' }
RemoteString class >> newString: aString onFileNumber: sourceIndex toFile: aFileStream [
	"Answer an instance of me for string, aString, on file indexed by 
	sourceIndex. Put the string on the file, aFileStream, and create the 
	remote reference. Assume that the index corresponds properly to 
	aFileStream."

	^self new string: aString onFileNumber: sourceIndex toFile: aFileStream
]

{ #category : #private }
RemoteString >> fileNumber: fileNumber position: position [ 

	sourceFileNumber := fileNumber.
	filePositionHi := position
]

{ #category : #accessing }
RemoteString >> last [
	^self string ifNotNil: [ :s | s last ] 
]

{ #category : #accessing }
RemoteString >> position [ 
	"Answer the location of the string on a file."

	^ filePositionHi 
]

{ #category : #accessing }
RemoteString >> sourceFileArray [
	^ sourceFileArray ifNil: [ SourceFiles ]
]

{ #category : #accessing }
RemoteString >> sourceFileArray: anObject [
	sourceFileArray := anObject
]

{ #category : #accessing }
RemoteString >> sourceFileNumber [
	"Answer the index of the file on which the string is stored."

	^ sourceFileNumber
]

{ #category : #accessing }
RemoteString >> sourcePointer [
	sourceFileNumber ifNil: [ ^ 0 ].
	^ SourceFiles sourcePointerFromFileIndex: sourceFileNumber andPosition: filePositionHi
]

{ #category : #accessing }
RemoteString >> string [ 
	"Answer the receiver's string if remote files are enabled, or nil."

	^ self sourceFileArray 
		readStreamAtFileIndex: sourceFileNumber 
		atPosition: filePositionHi 
		ifPresent: [ :readStream | (ChunkReadStream on: readStream) next ]
		ifAbsent: [ nil ]
]

{ #category : #private }
RemoteString >> string: aString [
	"Store this as my string if source files exist."

	self sourceFileArray changesWriteStreamDo: [ :theFile |
		theFile cr.
		self 
			string: aString 
			onFileNumber: 2
			toFile: theFile ]
]

{ #category : #chunk }
RemoteString >> string: aString onFileNumber: fileNumber toFile: aFileStream [ 
	"Store this as the receiver's string if source files exist."

	| position |
	position := aFileStream position.
	self fileNumber: fileNumber position: position.
	(ChunkWriteStream on: aFileStream) nextPut: aString.
	^ self 
]
