"
STONLargeWriteReadTests test the optimalizations for large structures.
"
Class {
	#name : 'STONLargeWriteReadTest',
	#superclass : 'STONWriteReadTest',
	#category : 'STON-Tests-Write-Read',
	#package : 'STON-Tests',
	#tag : 'Write-Read'
}

{ #category : 'private' }
STONLargeWriteReadTest >> reader: string [
	^ (super reader: string)
		yourself
]

{ #category : 'private' }
STONLargeWriteReadTest >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter
			on: stream;
			prettyPrint: true;
			nextPut: anObject ]
]
