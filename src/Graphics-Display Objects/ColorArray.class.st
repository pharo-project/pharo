"
An array of colors
"
Class {
	#name : 'ColorArray',
	#superclass : 'ArrayedCollection',
	#type : 'words',
	#category : 'Graphics-Display Objects-Utilities',
	#package : 'Graphics-Display Objects',
	#tag : 'Utilities'
}

{ #category : 'converting' }
ColorArray >> asColorArray [
	^self
]

{ #category : 'accessing' }
ColorArray >> at: index [

	| colorInteger |
	colorInteger := super at: index.
	^ Color colorFromPixelValue: colorInteger depth: 32
]

{ #category : 'accessing' }
ColorArray >> at: index put: aColor [
	^super at: index put: (aColor pixelWordForDepth: 32)
]
