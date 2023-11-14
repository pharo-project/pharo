"
Abstract class from package node displaying in the tree.
"
Class {
	#name : 'DAAbstractPackageNode',
	#superclass : 'DANode',
	#category : 'Tool-DependencyAnalyser-UI-Nodes',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Nodes'
}

{ #category : 'testing' }
DAAbstractPackageNode class >> isAbstract [

	^ self == DAAbstractPackageNode
]

{ #category : 'accessing' }
DAAbstractPackageNode >> packageName [
	^ self content target packageName
]

{ #category : 'browse-nautilus' }
DAAbstractPackageNode >> selectInBrowser [

	self selectPackage: self systemPackage
]

{ #category : 'accessing' }
DAAbstractPackageNode >> systemPackage [

	^ self content systemPackage
]
