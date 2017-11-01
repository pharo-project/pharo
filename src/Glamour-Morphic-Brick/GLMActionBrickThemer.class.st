Class {
	#name : #GLMActionBrickThemer,
	#superclass : #GLMBrickThemer,
	#category : 'Glamour-Morphic-Brick-Theme'
}

{ #category : #modules }
GLMActionBrickThemer >> popupThemer [

	^ self registerModule: GLMActionBrickPopupThemer new
]

{ #category : #brick-scrollpane }
GLMActionBrickThemer >> scrollbarWidth [

	^ 2
]

{ #category : #brick-scrollpane }
GLMActionBrickThemer >> scrollpaneBackgroundColor [

	^ Color transparent
]
