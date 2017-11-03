"
Abstract class used to display a group with its specific icons and no child in a tree.
"
Class {
	#name : #VSGroupLeafNode,
	#superclass : #VSGroupNode,
	#category : #Versionner-Spec-Browser
}

{ #category : #accessing }
VSGroupLeafNode >> children [
	^ [ #() ]

]

{ #category : #testing }
VSGroupLeafNode >> hasChildren [
	^ false
]
