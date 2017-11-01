"
STONLargeWriteReadTests test the optimalizations for large structures.
"
Class {
	#name : #STONLargeWriteReadTests,
	#superclass : #STONWriteReadTests,
	#category : 'STON-Tests'
}

{ #category : #private }
STONLargeWriteReadTests >> materialize: string [
	^ STON reader
		on: string readStream;
		optimizeForLargeStructures;
		next
]

{ #category : #private }
STONLargeWriteReadTests >> serialize: anObject [
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			optimizeForLargeStructures; 
			nextPut: anObject ]
]

{ #category : #private }
STONLargeWriteReadTests >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			optimizeForLargeStructures; 
			nextPut: anObject ]
]
