"
A PDPackageNode is used to display a package diff in a tree.
"
Class {
	#name : 'DAPackageItemDiffNode',
	#superclass : 'Object',
	#category : 'Tool-DependencyAnalyser-UI-Nodes',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
DAPackageItemDiffNode >> icon [
	self content isNew ifTrue: [ ^ self iconNamed: #changeAdd ].
	self content isRemoved ifTrue: [^ self iconNamed: #smallCancel].
	^ self iconNamed: #forward
]
