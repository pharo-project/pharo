Class {
	#name : #GLMBrickWrapper,
	#superclass : #GLMBrickGeneralWrapper,
	#category : #'Glamour-Morphic-Brick-Layouts-Utils'
}

{ #category : #api }
GLMBrickWrapper >> brickBounds: aRectangle [

	^ self brick brickBounds: aRectangle
]

{ #category : #api }
GLMBrickWrapper >> extent [

	^ self brick extent
]

{ #category : #api }
GLMBrickWrapper >> innerBounds [

	^ self brick innerBounds
]

{ #category : #api }
GLMBrickWrapper >> invalidateLayout [

	^ self brick invalidateLayout
]

{ #category : #api }
GLMBrickWrapper >> isHeightClean [

	^ self brick wrappedBounds isHeightClean
]

{ #category : #api }
GLMBrickWrapper >> isHeightDirty [

	^ self brick wrappedBounds isHeightDirty
]

{ #category : #api }
GLMBrickWrapper >> isWidthClean [

	^ self brick wrappedBounds isWidthClean
]

{ #category : #api }
GLMBrickWrapper >> isWidthDirty [

	^ self brick wrappedBounds isWidthDirty
]

{ #category : #api }
GLMBrickWrapper >> marginBottom [

	^ self brick marginBottom
]

{ #category : #api }
GLMBrickWrapper >> marginLeft [

	^ self brick marginLeft
]

{ #category : #api }
GLMBrickWrapper >> marginRight [

	^ self brick marginRight
]

{ #category : #api }
GLMBrickWrapper >> marginTop [

	^ self brick marginTop
]

{ #category : #api }
GLMBrickWrapper >> markHeightClean [

	^ self brick wrappedBounds markHeightClean
]

{ #category : #api }
GLMBrickWrapper >> markHeightDirty [

	^ self brick wrappedBounds markHeightDirty
]

{ #category : #api }
GLMBrickWrapper >> markLayoutClean [

	self layout markClean
]

{ #category : #api }
GLMBrickWrapper >> markLayoutDirty [

	self layout markDirty
]

{ #category : #api }
GLMBrickWrapper >> markWidthClean [

	^ self brick wrappedBounds markWidthClean
]

{ #category : #api }
GLMBrickWrapper >> markWidthDirty [

	^ self brick wrappedBounds markWidthDirty
]

{ #category : #api }
GLMBrickWrapper >> onChildrenLayouted [

	^ self brick onChildrenLayouted
]

{ #category : #api }
GLMBrickWrapper >> onLayoutEvent: aLayoutEvent [

	^ self brick onLayoutEvent: aLayoutEvent
]

{ #category : #api }
GLMBrickWrapper >> onLayouted [

	^ self brick onLayouted
]

{ #category : #api }
GLMBrickWrapper >> outerBounds [

	^ self brick outerBounds
]

{ #category : #api }
GLMBrickWrapper >> outerHeight: anInteger [

	^ self brick wrappedBounds outerHeight: anInteger
]

{ #category : #api }
GLMBrickWrapper >> outerOrigin: aPoint [

	^ self brick wrappedBounds outerOrigin: aPoint
]

{ #category : #api }
GLMBrickWrapper >> outerWidth: anInteger [

	^ self brick wrappedBounds outerWidth: anInteger
]

{ #category : #api }
GLMBrickWrapper >> paddingBottom [

	^ self brick paddingBottom
]

{ #category : #api }
GLMBrickWrapper >> paddingLeft [

	^ self brick paddingLeft
]

{ #category : #api }
GLMBrickWrapper >> paddingRight [

	^ self brick paddingRight
]

{ #category : #api }
GLMBrickWrapper >> paddingTop [

	^ self brick paddingTop
]

{ #category : #api }
GLMBrickWrapper >> translateX: anObject [

	^ self brick translateX: anObject
]

{ #category : #api }
GLMBrickWrapper >> translateY: anObject [

	^ self brick translateY: anObject
]

{ #category : #api }
GLMBrickWrapper >> width [

	^ self brick width
]

{ #category : #api }
GLMBrickWrapper >> withHeight: anInteger [

	^ self brick withHeight: anInteger
]

{ #category : #api }
GLMBrickWrapper >> withWidth: anInteger [

	^ self brick withWidth: anInteger
]
