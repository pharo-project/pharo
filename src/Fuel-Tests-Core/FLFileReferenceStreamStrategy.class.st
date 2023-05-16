"
I use binary read and write streams atop a memory filesystem.
"
Class {
	#name : #FLFileReferenceStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		#fileReference
	],
	#category : #'Fuel-Tests-Core-Support-StreamStrategies'
}

{ #category : #initialization }
FLFileReferenceStreamStrategy >> initialize [
	super initialize.
	
	fileReference := FileSystem memory / 'aFile'
]

{ #category : #reading }
FLFileReferenceStreamStrategy >> readStream [
	^ fileReference binaryReadStream
]

{ #category : #reading }
FLFileReferenceStreamStrategy >> writeStream [
	^ fileReference binaryWriteStream
]
