"
A PDCycleNode is used to display a extension dependency in a tree.
"
Class {
	#name : 'DAExtensionDependencyNode',
	#superclass : 'DAMethodDependencyNode',
	#category : 'Tool-DependencyAnalyser-UI-Nodes',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
DAExtensionDependencyNode >> icon [
	^  self iconNamed: #edit
]
