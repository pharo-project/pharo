"
A PDCycleNode is used to display a cycle in a tree.
"
Class {
	#name : 'DACycleNode',
	#superclass : 'DANode',
	#category : 'Tool-DependencyAnalyser-UI-Nodes',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
DACycleNode >> childNodeClassFor: anItem [
	^ DAPackageUnderAnalysisNode
]

{ #category : 'accessing' }
DACycleNode >> icon [
	^ self iconNamed: #refresh
]

{ #category : 'accessing' }
DACycleNode >> isCycleNode [
	^ true
]

{ #category : 'browse-nautilus' }
DACycleNode >> selectInBrowser [
	"we cannot browse a cycle in browser"
	^ self
]
