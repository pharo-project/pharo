"
STONWritePrettyPrinterReadTests tests pretty printed serialization followed by materialization.
"
Class {
	#name : #STONWritePrettyPrinterReadTest,
	#superclass : #STONWriteReadTest,
	#category : #'STON-Tests-Write-Read'
}

{ #category : #private }
STONWritePrettyPrinterReadTest >> serialize: anObject [
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
]

{ #category : #private }
STONWritePrettyPrinterReadTest >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
]
