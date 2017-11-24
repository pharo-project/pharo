Class {
	#name : #GLMBrickLayoutChildrenState,
	#superclass : #GLMBrickLayoutState,
	#category : #'Glamour-Morphic-Brick-Layouts'
}

{ #category : #testing }
GLMBrickLayoutChildrenState >> isOnChildren [
	^ true
]

{ #category : #layouting }
GLMBrickLayoutChildrenState >> layout: aBrick on: aTraverser [

	"we need to calculate sizes of all subbricks"
	aTraverser layoutChildrenOf: aBrick.
	"first we calculate corresponding dimension"
	aTraverser updateDimension: aBrick.
	"after everything is done we can mark dimension as clean"
	aTraverser markClean: aBrick.
	"because my dimension changed, maybe my owner would like to adapt"
	aTraverser layoutOwnerOf: aBrick
]

{ #category : #layouting }
GLMBrickLayoutChildrenState >> updateHeight: aBrick on: aLayout [

	aBrick withHeight: (aLayout normalizeHeight: (aLayout heightChildrenDependency: aBrick) of: aBrick)
]

{ #category : #layouting }
GLMBrickLayoutChildrenState >> updateWidth: aBrick on: aLayout [

	aBrick withWidth: (aLayout normalizeWidth: (aLayout widthChildrenDependency: aBrick) of: aBrick)
]
