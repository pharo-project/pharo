Class {
	#name : #GLMBrickHeightTraverser,
	#superclass : #GLMBrickLayoutTraverser,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Glamour-Morphic-Brick-Layouts-Utils'
}

{ #category : #cleanup }
GLMBrickHeightTraverser class >> cleanUp [

	uniqueInstance := nil
]

{ #category : #accessing }
GLMBrickHeightTraverser class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : #private }
GLMBrickHeightTraverser >> customDependencies: aBrick [

	^ aBrick layout heightDependencies
]

{ #category : #private }
GLMBrickHeightTraverser >> hasMaxDimensionRestriction: aBrick [

	^ aBrick layout hasMaxHeight
]

{ #category : #private }
GLMBrickHeightTraverser >> hasMinDimensionRestriction: aBrick [

	^ aBrick layout hasMinHeight
]

{ #category : #private }
GLMBrickHeightTraverser >> isClean: aBrick [

	^ aBrick isHeightClean
]

{ #category : #private }
GLMBrickHeightTraverser >> isOppositeClean: aBrick [

	^ aBrick isWidthClean
]

{ #category : #private }
GLMBrickHeightTraverser >> markClean: aBrick [

	aBrick markHeightClean
	
]

{ #category : #private }
GLMBrickHeightTraverser >> markDirty: aBrick [

	self markLayoutDirty: aBrick owner.
	aBrick markHeightDirty
]

{ #category : #accessing }
GLMBrickHeightTraverser >> opposite [

	^ GLMBrickWidthTraverser uniqueInstance
]

{ #category : #accessing }
GLMBrickHeightTraverser >> state [

	^ #vState
]

{ #category : #private }
GLMBrickHeightTraverser >> updateDimension: aBrick [
	"my height is already calculated, maybe because my children triggered calculation"
	aBrick isHeightClean ifTrue: [ ^ self ].
	
	(self dimensionPolicy: aBrick) updateHeight: aBrick
]
