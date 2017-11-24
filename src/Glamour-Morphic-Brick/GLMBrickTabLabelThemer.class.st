"
I'm a style module related to tab label
"
Class {
	#name : #GLMBrickTabLabelThemer,
	#superclass : #GLMBrickThemerModule,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-tab-label' }
GLMBrickTabLabelThemer >> normalBackgroundStyleFor: aBrick [

	^ Smalltalk ui theme tabLabelNormalFillStyleFor: aBrick
]

{ #category : #'brick-tab-label' }
GLMBrickTabLabelThemer >> normalBorderStyleFor: aBrick [

	^ Smalltalk ui theme tabLabelNormalBorderStyleFor: aBrick
]

{ #category : #'brick-tab-label' }
GLMBrickTabLabelThemer >> normalStyleFor: aBrick [

	aBrick fillStyle: (self normalBackgroundStyleFor: aBrick).
	aBrick borderStyle: (self normalBorderStyleFor: aBrick)
]

{ #category : #'brick-tab-label' }
GLMBrickTabLabelThemer >> selectedBackgroundStyleFor: aBrick [

	^ Smalltalk ui theme tabLabelSelectedFillStyleFor: aBrick
]

{ #category : #'brick-tab-label' }
GLMBrickTabLabelThemer >> selectedBorderStyleFor: aBrick [

	^ Smalltalk ui theme tabLabelSelectedBorderStyleFor: aBrick
]

{ #category : #'brick-tab-label' }
GLMBrickTabLabelThemer >> selectedStyleFor: aBrick [

	aBrick fillStyle: (self selectedBackgroundStyleFor: aBrick).
	aBrick borderStyle: (self selectedBorderStyleFor: aBrick)
]
