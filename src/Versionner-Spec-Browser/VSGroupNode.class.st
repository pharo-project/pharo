"
Abstract class used to display groups in a tree.
"
Class {
	#name : #VSGroupNode,
	#superclass : #TreeNodePresenter,
	#category : 'Versionner-Spec-Browser'
}

{ #category : #accessing }
VSGroupNode >> childNodeClassFor: anItem [
	"Use a specific wrapper node to display different icons."
	
	^ (self content project dependencyNamed: anItem) 
		ifNil: [ VSGroupLeafNode] "can be nil for 'default' or 'ALL'"
		ifNotNil: [ :dependency | dependency treeNodeClass ]
]

{ #category : #accessing }
VSGroupNode >> children [
	^ [ self content dependencies ]
]

{ #category : #testing }
VSGroupNode >> hasChildren [
	^ self content dependencies isNotEmpty
]

{ #category : #accessing }
VSGroupNode >> icon [
	^ self iconNamed: #groupIcon
]
