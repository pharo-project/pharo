"
I am a Morphic renderer for a GLMFastTreePresentation.

Description
--------------------

I use a GLMFastTreeDataSource to manage the data of the tree.

"
Class {
	#name : #GLMMorphicFastTreeRenderer,
	#superclass : #GLMMorphicFTRenderer,
	#category : #Glamour-FastTable-Morphic-Renderer
}

{ #category : #initialization }
GLMMorphicFastTreeRenderer >> createDataSourceFrom: aPresentation [
	^ GLMFastTreeDataSource new
]

{ #category : #initialization }
GLMMorphicFastTreeRenderer >> specificDataSourceInitializiation [
	| presentation |
	super specificDataSourceInitializiation.
	presentation := tableModel glamourPresentation.
	tableModel
		childrenBlock: presentation children;
		maxDepth: presentation maxDepth;
		searchStrategy: presentation strategy
]

{ #category : #initialization }
GLMMorphicFastTreeRenderer >> specificTableMorphInitializiation [
	| presentation |
	super specificTableMorphInitializiation.
	presentation := tableModel glamourPresentation.
	"If needed I extant the tree"
	presentation shouldAllExpand
		ifTrue: [ tableModel expandAll ].
	presentation shouldRootsExpand
		ifTrue: [ tableModel expandRoots ].
	presentation shouldExpandToLevel
		ifTrue: [ tableModel expandAllTo: presentation expandLevel ]
]
