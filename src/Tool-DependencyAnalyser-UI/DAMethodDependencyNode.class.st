"
A PDMethodNode is the superclass node for PDReferenceDependencyNode and PDExtensionDependencyNode. 
Indeed these two nodes have the same behavior to browse the dependency in the browser

"
Class {
	#name : #DAMethodDependencyNode,
	#superclass : #DANode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #'browse-nautilus' }
DAMethodDependencyNode >> selectInBrowser [
	self selectMethod: self content method
]
