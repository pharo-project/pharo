"
An array of colors
"
Class {
	#name : #ColorArray,
	#superclass : #ArrayedCollection,
	#type : #words,
	#category : #'Graphics-Display Objects'
}

{ #category : #converting }
ColorArray >> asColorArray [
	^self
]

{ #category : #accessing }
ColorArray >> at: index [
	^(super at: index) asColorOfDepth: 32
]

{ #category : #accessing }
ColorArray >> at: index put: aColor [
	^super at: index put: (aColor pixelWordForDepth: 32).
]
