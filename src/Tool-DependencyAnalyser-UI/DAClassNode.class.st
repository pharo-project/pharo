"
A PDClassDependencyNode is the superclass node for PDInheritanceNode and PDTraitDependencyNode. 
Indeed these two nodes have the same behavior to browse the dependency in the Nautilus UI.

"
Class {
	#name : #DAClassNode,
	#superclass : #DANode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #'browse-nautilus' }
DAClassNode >> browseInNautilus [
	self browseClassInNautilus: self classFromContent
]

{ #category : #'browse-nautilus' }
DAClassNode >> classFromContent [
	^ self content theClass
]
