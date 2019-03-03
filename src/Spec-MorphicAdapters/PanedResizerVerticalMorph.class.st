"
I'm a paned vertical resizer.
I'm called ""vertical"" (even if my draw is horizontal) because I split vertically the panes between me.

self exampleVertical.
"
Class {
	#name : #PanedResizerVerticalMorph,
	#superclass : #PanedResizerMorph,
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #accessing }
PanedResizerVerticalMorph >> edgeName [
	"Answer the value of edgeName"

	^ #bottom
]

{ #category : #testing }
PanedResizerVerticalMorph >> isHorizontal [
	"Answer true if the receiver has a horizontal layout."
	
	^ false
]

{ #category : #accessing }
PanedResizerVerticalMorph >> positionPoint: aPoint [
	"Reposition based on ptName."
	
	^ self position: self left @ aPoint y
]

{ #category : #private }
PanedResizerVerticalMorph >> setLayoutSizing [
	
	self 
		hResizing: #spaceFill; 
		vResizing: #rigid
]

{ #category : #private }
PanedResizerVerticalMorph >> setTargetBounds [
	| target |

	target := self target.
	target bounds: (target position corner: (target right @ (self top - 1)))
]

{ #category : #private }
PanedResizerVerticalMorph >> setTargetLayoutSizing [

	(self target vResizing = #rigid) ifTrue: [ ^ self ].
	self target vResizing: #rigid.
	self targetAfter vResizing: #spaceFill.
]
