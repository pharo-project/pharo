"
I am a world renderer for tests: when activated, I only remember whether the image was starting (`imageStarting`). Tests check with me how `WorldState` and `AbstractWorldRenderer` pass that flag down, without opening a window.

## Invariants and Assumptions

- I am never chosen to render a world: `isApplicableFor:` answers false.
"
Class {
	#name : 'WorldRendererMock',
	#superclass : 'AbstractWorldRenderer',
	#instVars : [
		'imageStarting'
	],
	#category : 'Morphic-Tests-Mocks',
	#package : 'Morphic-Tests',
	#tag : 'Mocks'
}

{ #category : 'testing' }
WorldRendererMock class >> isApplicableFor: aWorld [

	^ false
]

{ #category : 'activation' }
WorldRendererMock >> doActivateWhenImageStarting: isImageStarting [

	imageStarting := isImageStarting
]

{ #category : 'accessing' }
WorldRendererMock >> imageStarting [

	^ imageStarting
]
