"
I represent a file inside the iceberg repository. 

My contents are the contents of the file, utf8 decoded.

See my superclasses for more information.
"
Class {
	#name : #IceFileDefinition,
	#superclass : #IceFileSystemDefinition,
	#instVars : [
		'contents'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceFileDefinition class >> named: aString path: aPath contents: contents [
	
	^ (self named: aString)
		path: aPath;
		basicContents: contents
		yourself
]

{ #category : #'instance creation' }
IceFileDefinition class >> named: aString path: aPath fileReference: aFileReference [
	^ self
		named: aString
		path: aPath
		contents: (aFileReference binaryReadStreamDo: [ :each | each upToEnd ])
]

{ #category : #comparing }
IceFileDefinition >> = anotherDefinition [

	^ super = anotherDefinition and: [ contents = anotherDefinition basicContents ]
]

{ #category : #visiting }
IceFileDefinition >> accept: aVisitor [
	
	^ aVisitor visitFileNode: self
]

{ #category : #comparing }
IceFileDefinition >> basicContents [

	^ contents
]

{ #category : #accessing }
IceFileDefinition >> basicContents: anObject [

	contents := anObject
]

{ #category : #accessing }
IceFileDefinition >> contents [
	"Contents for display purposes.
	If the file is binary (or could not detect as utf8 file) then just return an hexa dump representation of the contents
	If the file is in utf8 decode it and show the text"

	^ self isBinary
		ifTrue: [ String streamContents: [ :stream | contents hexDumpOn: stream max: Float infinity ] ]
		ifFalse: [ contents utf8Decoded ]
]

{ #category : #accessing }
IceFileDefinition >> contents: anObject [

	contents := anObject
]

{ #category : #comparing }
IceFileDefinition >> hash [
	
	^ self contents hash
]

{ #category : #testing }
IceFileDefinition >> isBinary [
	
	[contents utf8Decoded] on: ZnInvalidUTF8 do: [ ^ true ].
	^ false
]

{ #category : #testing }
IceFileDefinition >> isFileDefinition [
	
	^ true
]

{ #category : #operations }
IceFileDefinition >> removeFrom: aVisitor [ 
	aVisitor removeFile: self
]
