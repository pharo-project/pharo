"
A PDCycleNode is used to display a inheritance dependency in a tree.
"
Class {
	#name : #DAInheritanceDependencyNode,
	#superclass : #DAClassNode,
	#category : #Tool-DependencyAnalyser-UI-Nodes
}

{ #category : #browse-nautilus }
DAInheritanceDependencyNode >> browseInOtherNautilus [
	self browseClass: self content superclass
]

{ #category : #accessing }
DAInheritanceDependencyNode >> icon [
	^ self iconNamed: #arrowUpIcon
]

{ #category : #accessing }
DAInheritanceDependencyNode >> isClassToDependencyNode [
	^ true
]
