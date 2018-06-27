"
i just a stub for rendering nothing , so that all update requests (like from Morhpic), are simply ignored
"
Class {
	#name : #OSNullFormRenderer,
	#superclass : #OSWindowFormRenderer,
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #size }
OSNullFormRenderer >> newExtent: anExtent [
]

{ #category : #'updating screen' }
OSNullFormRenderer >> updateAll [
	"Do nothing"
]

{ #category : #'morphic integration' }
OSNullFormRenderer >> updateAreas: allDamage immediate: forceToScreen [
	"Do Nothing"
]

{ #category : #'updating screen' }
OSNullFormRenderer >> updateRectangle: aRectangle [
	"Do nothing"
]
