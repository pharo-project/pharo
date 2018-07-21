"
STONWritePrettyPrinterReadTests tests pretty printed serialization followed by materialization.
"
Class {
	#name : #STONWritePrettyPrinterReadTests,
	#superclass : #STONWriteReadTests,
	#category : #'STON-Tests-Writer'
}

{ #category : #private }
STONWritePrettyPrinterReadTests >> serialize: anObject [
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
]

{ #category : #private }
STONWritePrettyPrinterReadTests >> serializeJson: anObject [
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
]
