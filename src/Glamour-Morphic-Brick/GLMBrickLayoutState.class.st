Class {
	#name : #GLMBrickLayoutState,
	#superclass : #Object,
	#category : 'Glamour-Morphic-Brick-Layouts'
}

{ #category : #testing }
GLMBrickLayoutState >> isOnChildren [

	^ false
]

{ #category : #testing }
GLMBrickLayoutState >> isOnMyself [

	^ false
]

{ #category : #testing }
GLMBrickLayoutState >> isOnParent [

	^ false
]

{ #category : #layouting }
GLMBrickLayoutState >> layout: aBrick on: aTraverser [
]

{ #category : #layouting }
GLMBrickLayoutState >> layoutPolicy: aBrick on: aTraverser [

	^ aBrick layoutPolicy
]

{ #category : #doing }
GLMBrickLayoutState >> on: aSymbol do: aBlock ifAbsent: absentBlock [
	|state|
	state := (aSymbol cull: self)
		ifTrue: [ self ]
		ifFalse: absentBlock.
	aBlock cull: state.
	^ state
]

{ #category : #doing }
GLMBrickLayoutState >> onChildrenDo: aBlock [

	^ self
		on: #isOnChildren
		do: aBlock
		ifAbsent: [ GLMBrickLayoutChildrenState new ]
]

{ #category : #doing }
GLMBrickLayoutState >> onMyselfDo: aBlock [

	^ self
		on: #isOnMyself
		do: aBlock
		ifAbsent: [ GLMBrickLayoutMyselfState new ]
]

{ #category : #doing }
GLMBrickLayoutState >> onParentDo: aBlock [

	^ self
		on: #isOnParent
		do: aBlock
		ifAbsent: [ GLMBrickLayoutParentState new ]
]

{ #category : #layouting }
GLMBrickLayoutState >> updateHeight: aBrick on: aLayout [
]

{ #category : #layouting }
GLMBrickLayoutState >> updateWidth: aBrick on: aLayout [
]
