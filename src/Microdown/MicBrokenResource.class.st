Class {
	#name : #MicBrokenResource,
	#superclass : #MicResourceReference,
	#category : #'Microdown-Core'
}

{ #category : #converting }
MicBrokenResource >> asFileReference [

	^ self error
]
