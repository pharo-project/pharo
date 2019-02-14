Class {
	#name : #GLMBrickWidthTraverser,
	#superclass : #GLMBrickLayoutTraverser,
	#traits : 'TSingleton',
	#classTraits : 'TSingleton classTrait',
	#category : #'Glamour-Morphic-Brick-Layouts-Utils'
}

{ #category : #cleanup }
GLMBrickWidthTraverser class >> cleanUp [

	self reset
]

{ #category : #private }
GLMBrickWidthTraverser >> customDependencies: aBrick [

	^ aBrick layout widthDependencies
]

{ #category : #private }
GLMBrickWidthTraverser >> hasMaxDimensionRestriction: aBrick [

	^ aBrick layout hasMaxWidth
]

{ #category : #private }
GLMBrickWidthTraverser >> hasMinDimensionRestriction: aBrick [

	^ aBrick layout hasMinWidth
]

{ #category : #private }
GLMBrickWidthTraverser >> isClean: aBrick [

	^ aBrick isWidthClean
]

{ #category : #private }
GLMBrickWidthTraverser >> isOppositeClean: aBrick [

	^ aBrick isHeightClean
]

{ #category : #private }
GLMBrickWidthTraverser >> markClean: aBrick [

	aBrick markWidthClean
	
]

{ #category : #private }
GLMBrickWidthTraverser >> markDirty: aBrick [

	self markLayoutDirty: aBrick owner.
	aBrick markWidthDirty
	
]

{ #category : #accessing }
GLMBrickWidthTraverser >> opposite [

	^ GLMBrickHeightTraverser uniqueInstance
]

{ #category : #accessing }
GLMBrickWidthTraverser >> state [

	^ #hState
]

{ #category : #private }
GLMBrickWidthTraverser >> updateDimension: aBrick [

	"my height is already calculated, maybe because it children triggered calculation"
	aBrick isWidthClean ifTrue: [ ^ self ].
	(self dimensionPolicy: aBrick)  updateWidth: aBrick
]
