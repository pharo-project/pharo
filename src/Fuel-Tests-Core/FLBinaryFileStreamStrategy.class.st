"
Specific to BinaryFileStream
"
Class {
	#name : #FLBinaryFileStreamStrategy,
	#superclass : #FLStreamStrategy,
	#category : #'Fuel-Tests-Core-StreamStrategies'
}

{ #category : #'test support' }
FLBinaryFileStreamStrategy >> fileName [
	"Answer a filename to serialize and materialize using it."

	^ FLPlatform current absolutePathOf: (FLPlatform current imageName, '-tests.fuel')
]

{ #category : #reading }
FLBinaryFileStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	^ FLPlatform current
		fileNamed: self fileName
		readStreamDo: [ :aStream |
			aValuable value: aStream binary ]
]

{ #category : #'test support' }
FLBinaryFileStreamStrategy >> tearDown [
	FLPlatform current deleteFileNamed: self fileName
]

{ #category : #writing }
FLBinaryFileStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."

	^ FLPlatform current
		fileNamed: self fileName
		writeStreamDo: [ :aStream | 
			aValuable value: aStream binary ]
]
