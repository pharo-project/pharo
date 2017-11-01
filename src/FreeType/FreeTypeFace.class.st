"
Do not rearrange these fields!
"
Class {
	#name : #FreeTypeFace,
	#superclass : #FT2Face,
	#instVars : [
		'filename',
		'index',
		'fileContentsExternalMemory',
		'hasKerning',
		'session'
	],
	#category : 'FreeType-Fonts'
}

{ #category : #'font dirs' }
FreeTypeFace class >> fontPathFor: aFilename [
	"aFilename is local. Try hard to return a valid path to be handed to freetype library"
	
	"temporary solution ;-)"
	^ (FileSystem disk workingDirectory / 'Fonts' / aFilename) fullName
]

{ #category : #'instance creation' }
FreeTypeFace class >> fromBytes: aByteArray index: anInteger [
	"share alike instances"
	| newFace |
	
	self allInstancesDo: [:inst |
		(inst fileContentsExternalMemoryBytes = aByteArray and: [inst index = anInteger])
			ifTrue: [^inst "validate"]].
	newFace := self basicNew
		fileContentsExternalMemory: (FreeTypeExternalMemory bytes: aByteArray);
		index: anInteger;
		yourself.
	self register: newFace.
	^ newFace
]

{ #category : #'instance creation' }
FreeTypeFace class >> fromFile: aFileName index: anInteger [
	"share alike instances"
	| file |
	file := aFileName asFileReference.
	self rememberFontDir: file parent fullName .
	
	self allInstancesDo: [:inst |
		(inst filename = aFileName and: [inst index = anInteger])
			ifTrue: [^inst "validate"]].
		
	^ (self basicNew)
		filename: aFileName;
		index: anInteger;
		yourself
]

{ #category : #'font dirs' }
FreeTypeFace class >> rememberFontDir: aDirecory [
]

{ #category : #initialize-release }
FreeTypeFace >> actAsExecutor [
	super actAsExecutor.
	filename := '<finalizer>'.
]

{ #category : #validation }
FreeTypeFace >> create [
	"create me in the FT2Plugin. This gets my handle, and loads the fields"
	
	fileContentsExternalMemory 
		ifNotNil: [ self newFaceFromExternalMemory: fileContentsExternalMemory index: index ]	
		ifNil: [	self newFaceFromFile: filename asFileReference index: index ].
	self loadFields

]

{ #category : #accessing }
FreeTypeFace >> familyName [
	^super familyName ifNil:['?']
]

{ #category : #accessing }
FreeTypeFace >> fileContentsExternalMemory: aFreeTypeExternalMemory [
	fileContentsExternalMemory := aFreeTypeExternalMemory
]

{ #category : #accessing }
FreeTypeFace >> fileContentsExternalMemoryBytes [
	^fileContentsExternalMemory ifNotNil:[fileContentsExternalMemory bytes]
]

{ #category : #accessing }
FreeTypeFace >> filename [
	^filename
]

{ #category : #accessing }
FreeTypeFace >> filename: aString [
	filename := aString
]

{ #category : #testing }
FreeTypeFace >> hasFamilyName [
	^super familyName notNil
]

{ #category : #testing }
FreeTypeFace >> hasKerning [
	^hasKerning ifNil:[
		[hasKerning := self primHasKerning = 64] 
			on: Error do:[:e | hasKerning := false].
		hasKerning]
]

{ #category : #testing }
FreeTypeFace >> hasStyleName [
	^super styleName notNil
]

{ #category : #accessing }
FreeTypeFace >> index [
	^index
]

{ #category : #accessing }
FreeTypeFace >> index: anInteger [
	index := anInteger
]

{ #category : #caching }
FreeTypeFace >> releaseCachedState [

	hasKerning := nil.

]

{ #category : #accessing }
FreeTypeFace >> styleName [
	^super styleName ifNil:['']
]

{ #category : #validation }
FreeTypeFace >> validate [
	"If the receiver is not valid (has a nil handle), then create the 
	receiver to obtain a handle and load the receiver's fields"
	
	(session == Smalltalk session 
		and: [ self isValid 
		and: [ numCharmaps notNil ] ]) 
		ifTrue: [ ^ self ].
	
	session := Smalltalk session.
	handle := nil.
	self create
]
