"
A PDCycleNode is used to display a reference dependency in a tree.
"
Class {
	#name : #DAReferenceDependencyNode,
	#superclass : #DAMethodDependencyNode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

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

{ #category : #'browse-nautilus' }
DAReferenceDependencyNode >> spawnNewBrowser [
	| classToBrowse |
	classToBrowse := self content reference.
	self browseClass: classToBrowse
]
