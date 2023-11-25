Class {
	#name : 'ScalingCanvasSavedPatch',
	#superclass : 'Object',
	#instVars : [
		'form',
		'scale'
	],
	#category : 'Morphic-Core-Kernel',
	#package : 'Morphic-Core',
	#tag : 'Kernel'
}

{ #category : 'instance creation' }
ScalingCanvasSavedPatch class >> form: form scale: scale [

	^ self basicNew initializeWithForm: form scale: scale
]

{ #category : 'accessing' }
ScalingCanvasSavedPatch >> extent [

	^ form extent / scale
]

{ #category : 'accessing' }
ScalingCanvasSavedPatch >> form [

	^ form
]

{ #category : 'initialization' }
ScalingCanvasSavedPatch >> initializeWithForm: initialForm scale: initialScale [

	self initialize.
	form := initialForm.
	scale := initialScale.
]

{ #category : 'accessing' }
ScalingCanvasSavedPatch >> offset [

	^ form offset / scale
]

{ #category : 'accessing' }
ScalingCanvasSavedPatch >> offset: newOffset [

	form offset: newOffset * scale
]
