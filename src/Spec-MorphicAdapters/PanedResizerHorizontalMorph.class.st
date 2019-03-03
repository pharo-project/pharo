"
I'm a paned horizontal resizer.
I'm called ""horizonal"" (even if my draw is vertical) because I split horizontally the panes between me.

self exampleHorizontal.
"
Class {
	#name : #PanedResizerHorizontalMorph,
	#superclass : #PanedResizerMorph,
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #accessing }
PanedResizerHorizontalMorph >> edgeName [
	"Answer the value of edgeName"

	^ #right
]

{ #category : #testing }
PanedResizerHorizontalMorph >> isHorizontal [
	"Answer true if the receiver has a horizontal layout."
	
	^ true
]

{ #category : #accessing }
PanedResizerHorizontalMorph >> positionPoint: aPoint [
	"Reposition based on ptName."
	
	^ self position: aPoint x @ self top
]

{ #category : #private }
PanedResizerHorizontalMorph >> setLayoutSizing [
	"Adjust the sizing for use within table layouts."

	self 
		hResizing: #rigid; 
		vResizing: #spaceFill
]

{ #category : #private }
PanedResizerHorizontalMorph >> setTargetBounds [
	| target |
	
	target := self target.
	target bounds: (target bounds withRight: self left - 1)
]

{ #category : #private }
PanedResizerHorizontalMorph >> setTargetLayoutSizing [

	(self target hResizing = #rigid) ifTrue: [ ^ self ].
	self target hResizing: #rigid.
	self targetAfter hResizing: #spaceFill.
]
