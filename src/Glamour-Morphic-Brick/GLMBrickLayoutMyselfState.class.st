Class {
	#name : #GLMBrickLayoutMyselfState,
	#superclass : #GLMBrickLayoutState,
	#instVars : [
		'dimension'
	],
	#category : 'Glamour-Morphic-Brick-Layouts'
}

{ #category : #initialization }
GLMBrickLayoutMyselfState >> defaultDimension [

	^ 50
]

{ #category : #accessing }
GLMBrickLayoutMyselfState >> dimension [
	^ dimension
]

{ #category : #accessing }
GLMBrickLayoutMyselfState >> dimension: anObject [
	dimension := anObject
]

{ #category : #initialization }
GLMBrickLayoutMyselfState >> initialize [
	super initialize.
	
	dimension := self defaultDimension
]

{ #category : #testing }
GLMBrickLayoutMyselfState >> isOnMyself [

	^ true
]

{ #category : #layouting }
GLMBrickLayoutMyselfState >> layout: aBrick on: aTraverser [

	"immidiately set my dimension"
	aTraverser updateDimension: aBrick.
	aTraverser markClean: aBrick.
	aTraverser layoutChildrenOf: aBrick.
	aTraverser layoutOwnerOf: aBrick
]

{ #category : #layouting }
GLMBrickLayoutMyselfState >> updateHeight: aBrick on: aLayout [

	aBrick withHeight: (aLayout normalizeHeight: (aLayout heightMyselfDependency: aBrick) of: aBrick)
]

{ #category : #layouting }
GLMBrickLayoutMyselfState >> updateWidth: aBrick on: aLayout [

	aBrick withWidth: (aLayout normalizeWidth: (aLayout widthMyselfDependency: aBrick) of: aBrick)
]
