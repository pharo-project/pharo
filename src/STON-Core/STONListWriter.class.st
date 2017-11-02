"
STONArrayWriter helps in writing array based STON representations.

"
Class {
	#name : #STONListWriter,
	#superclass : #STONStreamWriter,
	#category : 'STON-Core'
}

{ #category : #accessing }
STONListWriter >> add: anObject [
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject
]
