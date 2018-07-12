"
Unit test writing and reading of comments
"
Class {
	#name : #STONWriteReadCommentsTests,
	#superclass : #STONWriteReadTests,
	#category : #'STON-Tests-Writer'
}

{ #category : #private }
STONWriteReadCommentsTests >> materialize: string [
	^ STON reader
		on: (STONCStyleCommentsSkipStream on: string readStream);
		optimizeForLargeStructures;
		next
]

{ #category : #private }
STONWriteReadCommentsTests >> serialize: anObject [
	^ String streamContents: [ :stream |
		stream << '/* initial comment */'.
		STON writer 
			on: stream;
			nextPut: anObject.
		stream << '/* final comment */' ]
]
