"
STONShortArrayWriter helps in writing short array based STON representations.

"
Class {
	#name : 'STONShortListWriter',
	#superclass : 'STONListWriter',
	#category : 'STON-Core-Writer',
	#package : 'STON-Core',
	#tag : 'Writer'
}

{ #category : 'accessing' }
STONShortListWriter >> add: anObject [
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject
]
