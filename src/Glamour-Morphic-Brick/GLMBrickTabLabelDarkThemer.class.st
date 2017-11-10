"
I'm a style module related to tab label modified for Dark Theme
"
Class {
	#name : #GLMBrickTabLabelDarkThemer,
	#superclass : #GLMBrickTabLabelThemer,
	#category : #Glamour-Morphic-Brick-Theme
}

{ #category : #brick-tab-label }
GLMBrickTabLabelDarkThemer >> normalBorderStyleFor: aBrick [

	^ BorderStyle simple
			width: 1;
			baseColor: Color transparent
]

{ #category : #brick-tab-label }
GLMBrickTabLabelDarkThemer >> normalStyleFor: aBrick [

	super normalStyleFor: aBrick.
	aBrick padding: #(0 1).
]

{ #category : #brick-tab-label }
GLMBrickTabLabelDarkThemer >> selectedBorderStyleFor: aBrick [

	^ self normalBorderStyleFor: aBrick
]
