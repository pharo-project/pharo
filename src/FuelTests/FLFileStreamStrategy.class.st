"
I am a strategy for traditional file streams.
"
Class {
	#name : #FLFileStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		'fileStreamClass'
	],
	#category : #'FuelTests-StreamStrategies'
}

{ #category : #'instance creation' }
FLFileStreamStrategy class >> new [
	self error: 'Invalid creation method.'
]

{ #category : #'instance creation' }
FLFileStreamStrategy class >> newWith: aFileStream [

	^self basicNew
		initializeWith: aFileStream;
		yourself
]

{ #category : #'instance creation' }
FLFileStreamStrategy class >> newWithBinaryFileStream [
	^ (FLPlatform current isPharo and: [
		FLPlatform current versionNumber >= 5 ])
			ifTrue: [
				FLBinaryFileStreamStrategy basicNew
					initialize;
					yourself ]
			ifFalse: [ self newWithStandardFileStream ]
]

{ #category : #'instance creation' }
FLFileStreamStrategy class >> newWithMultiByteFileStream [

	^self newWith: MultiByteFileStream
]

{ #category : #'instance creation' }
FLFileStreamStrategy class >> newWithStandardFileStream [

	^self newWith: StandardFileStream
]

{ #category : #writing }
FLFileStreamStrategy >> fileName [
	"Answer a filename to serialize and materialize using it."

	^ FLPlatform current absolutePathOf: (FLPlatform current imageName, '-tests.fuel')
]

{ #category : #writing }
FLFileStreamStrategy >> fileStreamClass [
	"Returns the FileStream specific class"
	
	^fileStreamClass
]

{ #category : #writing }
FLFileStreamStrategy >> initializeWith: aFileStreamClass [

	self initialize.	
	fileStreamClass := aFileStreamClass.
]

{ #category : #reading }
FLFileStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	^self fileStreamClass
		oldFileNamed: self fileName
		do: [ :aStream | aValuable value: aStream binary ]
]

{ #category : #'test support' }
FLFileStreamStrategy >> tearDown [
	(File named: self fileName) delete
]

{ #category : #writing }
FLFileStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."

	^self fileStreamClass
		forceNewFileNamed: self fileName 
		do: [ :aStream | aValuable value: aStream binary ]
]
