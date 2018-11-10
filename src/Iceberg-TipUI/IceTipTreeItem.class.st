"
I'm a leaf element of a tree.
"
Class {
	#name : #IceTipTreeItem,
	#superclass : #IceTipTreeNode,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #accessing }
IceTipTreeItem >> descriptionWithDecoration [
	^ self model descriptionWithDecoration
]

{ #category : #testing }
IceTipTreeItem >> isSameAs: aNode [
	^ aNode species = self species 
		and: [ self model = aNode model ]
]

{ #category : #accessing }
IceTipTreeItem >> traversingDo: aBlock parents: anArray [
	aBlock cull: self cull: anArray
]
