"
I am an `OSWorldRenderer` for tests: when activated, I open no OS window and only remember whether the image was starting (`imageStarting`). Tests check with me that `doActivate`, sent when a native window opens, tells that the image is not starting.

## Invariants and Assumptions

- I am never chosen to render a world: `isApplicableFor:` answers false.
"
Class {
	#name : 'OSWorldRendererMock',
	#superclass : 'OSWorldRenderer',
	#instVars : [
		'imageStarting'
	],
	#category : 'OSWindow-Tests-Mocks',
	#package : 'OSWindow-Tests',
	#tag : 'Mocks'
}

{ #category : 'accessing' }
OSWorldRendererMock class >> isApplicableFor: aWorld [

	^ false
]

{ #category : 'activation' }
OSWorldRendererMock >> doActivateWhenImageStarting: isImageStarting [

	imageStarting := isImageStarting
]

{ #category : 'accessing' }
OSWorldRendererMock >> imageStarting [

	^ imageStarting
]
