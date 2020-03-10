"
I use binary read and write streams atop a memory filesystem.
"
Class {
	#name : #FLFileReferenceStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		'reference'
	],
	#category : #'Fuel-Tests-Core-StreamStrategies'
}

{ #category : #initialization }
FLFileReferenceStreamStrategy >> initialize [
	super initialize.
	
	reference := FileSystem memory / 'aFile'
]

{ #category : #reading }
FLFileReferenceStreamStrategy >> readStreamDo: aValuable [
	^ reference binaryReadStreamDo: [ :stream | aValuable value: stream ]
]

{ #category : #writing }
FLFileReferenceStreamStrategy >> writeStreamDo: aValuable [
	^ reference binaryWriteStreamDo: [ :stream | aValuable value: stream ]
]
