"
Abstract class from package node displaying in the tree.
"
Class {
	#name : #DAAbstractPackageNode,
	#superclass : #DANode,
	#category : #Tool-DependencyAnalyser-UI-Nodes
}

{ #category : #browse-nautilus }
DAAbstractPackageNode >> browseInNautilus [
	self browsePackageInNautilus: self rPackage.
]

{ #category : #accessing }
DAAbstractPackageNode >> packageName [
	^ self content target packageName
]
