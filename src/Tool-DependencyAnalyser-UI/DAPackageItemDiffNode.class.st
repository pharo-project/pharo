"
A PDPackageNode is used to display a package diff in a tree.
"
Class {
	#name : #DAPackageItemDiffNode,
	#superclass : #TreeNodePresenter,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #accessing }
DAPackageItemDiffNode >> icon [
	self content isNew ifTrue: [ ^ self iconNamed: #changeAddIcon ].
	self content isRemoved ifTrue: [^ self iconNamed: #smallCancelIcon ].
	^ self iconNamed: #forwardIcon
]
