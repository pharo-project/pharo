"
A PDCycleNode is used to display a reference dependency in a tree.
"
Class {
	#name : #DAReferenceDependencyNode,
	#superclass : #DAMethodDependencyNode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #'browse-nautilus' }
DAReferenceDependencyNode >> browseInOtherNautilus [
	| classToBrowse |
	classToBrowse := self content reference.
	self browseClass: classToBrowse
]

{ #category : #testing }
DAReferenceDependencyNode >> browseSendersOrUsers [
	
]

{ #category : #accessing }
DAReferenceDependencyNode >> icon [
	^ self iconNamed: #classIcon
]

{ #category : #testing }
DAReferenceDependencyNode >> isClassToDependencyNode [
	^ true
]

{ #category : #testing }
DAReferenceDependencyNode >> isReferenceNode [
	  ^ true
]
