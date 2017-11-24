Class {
	#name : #GLMMorphicTreeRenderer,
	#superclass : #GLMMorphicListingRenderer,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicTreeRenderer >> render: aPresentation [
	| container |
	container := super render: aPresentation.
	aPresentation shouldAllExpand ifTrue: [treeMorph expandAll ].
	aPresentation shouldRootsExpand ifTrue: [treeMorph expandRoots ].
	aPresentation shouldExpandToLevel ifTrue: [ treeMorph expandAllTo: aPresentation expandLevel ].
	^ container
]
