"
A group node in the morph tree
"
Class {
	#name : #GroupNode,
	#superclass : #MorphTreeNodeModel,
	#category : #'Nautilus-GroupManagerUI'
}

{ #category : #accessing }
GroupNode >> childNodeClassFromItem: anItem [

	^ ItemNode
]

{ #category : #accessing }
GroupNode >> childrenItems [

	^ self item elements
]

{ #category : #accessing }
GroupNode >> icon [
	^ self iconNamed: #publishIcon
]

{ #category : #testing }
GroupNode >> isGroup [

	^ true
]

{ #category : #accessing }
GroupNode >> rowMorphForColumn: aTreeColumn [

	^ self item ifNotNil: [:i | i name asMorph] 
]
