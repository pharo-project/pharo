"
Multiple selections in a morphic based tree
"
Class {
	#name : #MorphTreeMorphMultipleSelection,
	#superclass : #MorphTreeMorphSelection,
	#instVars : [
		'selectedNodePathList'
	],
	#category : #Morphic-Widgets-Tree
}

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> addNodePath: aPath [
	self selectedNodePathList add: aPath
]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> empty [
	self selectedNodePathList: OrderedCollection new

]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> lastSelectedNodePath [
	^ self selectedNodePathList ifNotEmpty: [:l | l last]
]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> removeNode: aNode [
	self selectedNodePathList remove: aNode path ifAbsent: []

]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> selectedItemOrItemsOrNil [
	| theItems |
	theItems := self selectedNodeList collect: [ :each | each item ].
	^ theItems isEmpty ifTrue: [nil] ifFalse: [ theItems ]
]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> selectedNodeList [
	^ self selectedNodePathList 
		ifNil: [#()]
		ifNotNil: [:l | l collect: [:path | path last]]
]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> selectedNodePathList [
	^ selectedNodePathList ifNil: [selectedNodePathList := OrderedCollection new]
]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> selectedNodePathList: aCollectionOfPath [
	selectedNodePathList := aCollectionOfPath
]

{ #category : #accessing }
MorphTreeMorphMultipleSelection >> selectedNodes [
	^ OrderedCollection withAll: self selectedNodeList

]
