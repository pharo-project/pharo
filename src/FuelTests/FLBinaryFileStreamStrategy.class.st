"
Specific to BinaryFileStream
"
Class {
	#name : #FLBinaryFileStreamStrategy,
	#superclass : #FLFileStreamStrategy,
	#category : #'FuelTests-StreamStrategies'
}

{ #category : #reading }
FLBinaryFileStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	^ ((Smalltalk globals at: #File)
		named: self fileName) readStreamDo: [ :aStream |
			aValuable value: aStream ]
]

{ #category : #writing }
FLBinaryFileStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."

	^ ((Smalltalk globals at: #File)
		named: self fileName) writeStreamDo: [ :aStream | 
			aValuable value: aStream ]
]
