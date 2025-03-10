"
Specific to BinaryFileStream
"
Class {
	#name : #FLBinaryFileStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		#fileName
	],
	#category : #'Fuel-Tests-Core-Support-StreamStrategies'
}

{ #category : #accessing }
FLBinaryFileStreamStrategy >> fileName [
	^ fileName
]

{ #category : #initialization }
FLBinaryFileStreamStrategy >> initialize [
	super initialize.
	
	fileName := UUID new asString
]

{ #category : #reading }
FLBinaryFileStreamStrategy >> readStream [
	^ FLFilePoolTestResource current readStreamForFilePath: self fileName
]

{ #category : #writing }
FLBinaryFileStreamStrategy >> writeStream [
	^ FLFilePoolTestResource current writeStreamForFilePath: self fileName
]
