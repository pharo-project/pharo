"
I am a strategy for MultiByteBinaryOrTextStream..
"
Class {
	#name : #FLMultiByteStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		'memoryFileSystem'
	],
	#category : #'Fuel-Tests-Core-StreamStrategies'
}

{ #category : #writing }
FLMultiByteStreamStrategy >> initialize [

	super initialize.
	memoryFileSystem := FileSystem memory 
]

{ #category : #reading }
FLMultiByteStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	^ (memoryFileSystem / 'temp')
		binaryReadStreamDo: [ :stream | aValuable value: stream ] 
]

{ #category : #writing }
FLMultiByteStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."

	^ (memoryFileSystem / 'temp')
		binaryWriteStreamDo: [ :stream | aValuable value: stream ]
]
