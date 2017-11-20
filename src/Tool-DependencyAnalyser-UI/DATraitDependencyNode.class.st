"
A PDCycleNode is used to display a trait dependency in a tree.
"
Class {
	#name : #DATraitDependencyNode,
	#superclass : #DAClassNode,
	#category : #Tool-DependencyAnalyser-UI-Nodes
}

{ #category : #browse-nautilus }
DATraitDependencyNode >> browseInOtherNautilus [
	self browseClass: self content trait
]

{ #category : #accessing }
DATraitDependencyNode >> icon [
	^  Smalltalk ui icons iconNamed: #traitIcon
]
