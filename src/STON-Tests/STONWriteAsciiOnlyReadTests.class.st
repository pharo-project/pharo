Class {
	#name : #STONWriteAsciiOnlyReadTests,
	#superclass : #STONWriteReadTests,
	#category : #STON-Tests
}

{ #category : #private }
STONWriteAsciiOnlyReadTests >> serialize: anObject [
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			asciiOnly: true;
			nextPut: anObject ]
]

{ #category : #private }
STONWriteAsciiOnlyReadTests >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			asciiOnly: true;
			nextPut: anObject ]
]
