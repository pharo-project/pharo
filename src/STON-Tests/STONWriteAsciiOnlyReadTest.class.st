Class {
	#name : #STONWriteAsciiOnlyReadTest,
	#superclass : #STONWriteReadTest,
	#category : #'STON-Tests-Write-Read'
}

{ #category : #private }
STONWriteAsciiOnlyReadTest >> serialize: anObject [
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			asciiOnly: true;
			nextPut: anObject ]
]

{ #category : #private }
STONWriteAsciiOnlyReadTest >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			asciiOnly: true;
			nextPut: anObject ]
]
