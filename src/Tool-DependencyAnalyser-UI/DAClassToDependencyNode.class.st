Class {
	#name : #DAClassToDependencyNode,
	#superclass : #DAClassNode,
	#instVars : [
		'package'
	],
	#category : #Tool-DependencyAnalyser-UI-Nodes
}

{ #category : #browse-nautilus }
DAClassToDependencyNode >> browseInOtherNautilus [
	self browseClass: self content browse
]

{ #category : #procotol }
DAClassToDependencyNode >> childNodeClassFor: anItem [
	 ^ anItem nodeClass
]

{ #category : #accessing }
DAClassToDependencyNode >> children [
	^ self dependencies
]

{ #category : #accessing }
DAClassToDependencyNode >> dependencies [
	^ self package dependenciesFrom: self content
]

{ #category : #accessing }
DAClassToDependencyNode >> hasChildren [
	^ self dependencies isNotEmpty
]

{ #category : #accessing }
DAClassToDependencyNode >> icon [
	^ self iconNamed: #confirmIcon
]

{ #category : #accessing }
DAClassToDependencyNode >> package [
	^ package
]

{ #category : #accessing }
DAClassToDependencyNode >> package: anObject [
	package := anObject
]
