"
STONLargeWriteReadTests test the optimalizations for large structures.
"
Class {
	#name : #STONLargeWriteReadTest,
	#superclass : #STONWriteReadTest,
	#category : #'STON-Tests-Writer'
}

{ #category : #private }
STONLargeWriteReadTest >> materialize: string [
	^ STON reader
		on: string readStream;
		optimizeForLargeStructures;
		next
]

{ #category : #private }
STONLargeWriteReadTest >> serialize: anObject [
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			optimizeForLargeStructures; 
			nextPut: anObject ]
]

{ #category : #private }
STONLargeWriteReadTest >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			optimizeForLargeStructures; 
			nextPut: anObject ]
]
