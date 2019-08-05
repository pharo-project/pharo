"
Superclass for selections in a morphic based tree
"
Class {
	#name : #MorphTreeMorphSelection,
	#superclass : #Object,
	#category : #'Morphic-Widgets-Tree'
}

{ #category : #accessing }
MorphTreeMorphSelection >> addNode: aNode [
	self addNodePath: aNode path
]

{ #category : #accessing }
MorphTreeMorphSelection >> addNodePath: aPath [
	^ self subclassResponsibility 
	
]

{ #category : #accessing }
MorphTreeMorphSelection >> empty [
	self subclassResponsibility 
]

{ #category : #accessing }
MorphTreeMorphSelection >> lastSelectedNode [
	^ self lastSelectedNodePath ifNotNil: [:path | path ifEmpty: [nil] ifNotEmpty: [:p | path last]]
]

{ #category : #accessing }
MorphTreeMorphSelection >> lastSelectedNodePath [ 
	^ self subclassResponsibility
]

{ #category : #accessing }
MorphTreeMorphSelection >> removeNode: aNode [
	self subclassResponsibility 
]

{ #category : #accessing }
MorphTreeMorphSelection >> selectedItemOrItemsOrNil [

	^ self lastSelectedNode ifNil: [ nil ] ifNotNil: [ self lastSelectedNode item ]
]

{ #category : #accessing }
MorphTreeMorphSelection >> selectedItems [
	^ self selectedNodes collect: [:n | n item]

]

{ #category : #accessing }
MorphTreeMorphSelection >> selectedNodePathList [
	^ self subclassResponsibility
]

{ #category : #accessing }
MorphTreeMorphSelection >> selectedNodes [
	^ self lastSelectedNode
		ifNotNil: [:l | OrderedCollection with: l]
		ifNil: [OrderedCollection new]
]

{ #category : #'view updating' }
MorphTreeMorphSelection >> updateView: aTreeMorph forModel: aTreeModel [
	aTreeMorph updateFromSelection: self.
	aTreeModel selectionChanged
]
