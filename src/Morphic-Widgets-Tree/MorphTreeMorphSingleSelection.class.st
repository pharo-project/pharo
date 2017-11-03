"
Single selections in a morphic based tree
"
Class {
	#name : #MorphTreeMorphSingleSelection,
	#superclass : #MorphTreeMorphSelection,
	#instVars : [
		'selectedNodePath'
	],
	#category : #Morphic-Widgets-Tree
}

{ #category : #accessing }
MorphTreeMorphSingleSelection >> addNodePath: aPath [
	self selectedNodePath: aPath
]

{ #category : #accessing }
MorphTreeMorphSingleSelection >> empty [
	self selectedNodePath: nil
]

{ #category : #accessing }
MorphTreeMorphSingleSelection >> lastSelectedNodePath [
	^ self selectedNodePath
]

{ #category : #accessing }
MorphTreeMorphSingleSelection >> removeNode: aNode [
	self selectedNodePath = aNode path
		ifTrue: [self selectedNodePath: nil]
]

{ #category : #accessing }
MorphTreeMorphSingleSelection >> selectedNodePath [
	^ selectedNodePath
]

{ #category : #accessing }
MorphTreeMorphSingleSelection >> selectedNodePath: aSelectionPath [
	selectedNodePath := aSelectionPath
]

{ #category : #accessing }
MorphTreeMorphSingleSelection >> selectedNodePathList [
	^ self selectedNodePath ifNil: [#()] ifNotNil: [Array with: self selectedNodePath]
]
