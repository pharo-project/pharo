"
An item in the group UI
"
Class {
	#name : #ItemNode,
	#superclass : #MorphTreeNodeModel,
	#category : #Nautilus-GroupManagerUI
}

{ #category : #accessing }
ItemNode >> icon [
	^ self iconNamed: #packageIcon
]

{ #category : #testing }
ItemNode >> isGroup [

	^ false
]

{ #category : #testing }
ItemNode >> rowMorphForColumn: aTreeColumn [

	^ self item ifNotNil: [:i | i printString asMorph] 
]
