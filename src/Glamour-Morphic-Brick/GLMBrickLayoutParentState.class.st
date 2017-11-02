Class {
	#name : #GLMBrickLayoutParentState,
	#superclass : #GLMBrickLayoutState,
	#instVars : [
		'percent'
	],
	#category : 'Glamour-Morphic-Brick-Layouts'
}

{ #category : #initialization }
GLMBrickLayoutParentState >> defaultPercent [

	^ 100
]

{ #category : #initialization }
GLMBrickLayoutParentState >> initialize [
	super initialize.
	
	percent := self defaultPercent
]

{ #category : #testing }
GLMBrickLayoutParentState >> isOnParent [
	^ true
]

{ #category : #layouting }
GLMBrickLayoutParentState >> layout: aBrick on: aTraverser [

	aBrick owner ifNil: [ ^ aTraverser markClean: aBrick ].
	(aTraverser isDependsOnChildren: aBrick owner) ifTrue: [ Error signal: 'Circle dependency detected!' ].
	
	"because I depend on my parent, it should be layouted first"
	aTraverser layoutOwnerOf: aBrick.
	"then I can update my dimension"
	aTraverser updateDimension: aBrick.
	"everething is done I can mark dimension as clean"
	aTraverser markClean: aBrick.
	"and I can say my children to layout themselves, because I'm already clean"
	aTraverser layoutChildrenOf: aBrick
]

{ #category : #layouting }
GLMBrickLayoutParentState >> layoutPolicy: aBrick on: aTraverser [

	^ (aBrick owner
		ifNil: [ aBrick ]
		ifNotNil: [ :owner |
			owner isBrick
				ifTrue: [ owner ]
				ifFalse: [ aBrick ] ]) layoutPolicy
]

{ #category : #accessing }
GLMBrickLayoutParentState >> percent [
	^ percent
]

{ #category : #accessing }
GLMBrickLayoutParentState >> percent: anObject [
	percent := anObject
]

{ #category : #layouting }
GLMBrickLayoutParentState >> updateHeight: aBrick on: aLayout [

	aBrick outerHeight: (aLayout normalizeHeight: (aLayout heightParentDependency: aBrick) of: aBrick)
]

{ #category : #layouting }
GLMBrickLayoutParentState >> updateWidth: aBrick on: aLayout [

	aBrick outerWidth: (aLayout normalizeWidth: (aLayout widthParentDependency: aBrick) of: aBrick)
]
