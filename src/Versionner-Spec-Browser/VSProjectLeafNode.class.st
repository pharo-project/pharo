"
Abstract class used to display a project with its specific icons and no child in a tree.
"
Class {
	#name : #VSProjectLeafNode,
	#superclass : #VSProjectNode,
	#category : #'Versionner-Spec-Browser'
}

{ #category : #accessing }
VSProjectLeafNode >> children [
	^ [ #() ]

]

{ #category : #testing }
VSProjectLeafNode >> hasChildren [
	^ false
]
