"
BFS (see http://en.wikipedia.org/wiki/Breadth-first_search)  iterator for an MTProject graph.
We avoid to visit already visited children of a node.
"
Class {
	#name : #MTProjectBFSIterator,
	#superclass : #Object,
	#instVars : [
		'project',
		'queue',
		'alreadyVisited'
	],
	#category : #'Versionner-Core-DependenciesModel'
}

{ #category : #'instance creation' }
MTProjectBFSIterator class >> on: anMTProject [
	^ self new
		project: anMTProject;
		yourself.
]

{ #category : #private }
MTProjectBFSIterator >> hasChildNodes: aNode [
	| dependencies |
	
	[ dependencies := aNode dependencies ] 
		on: MessageNotUnderstood 
		do: [  ^false ].
	^ dependencies size > 0
]

{ #category : #protocol }
MTProjectBFSIterator >> hasNext [
	^ queue isEmpty not
	
]

{ #category : #initialization }
MTProjectBFSIterator >> initialize [
	queue := SharedQueue new.
	alreadyVisited := Set new.
]

{ #category : #private }
MTProjectBFSIterator >> isNotVisited: aNode [
	^ (alreadyVisited includes: aNode) not
]

{ #category : #protocol }
MTProjectBFSIterator >> next [
	| currentElement |
	
	currentElement := queue nextOrNil.
	currentElement ifNotNil: [
		((self hasChildNodes: currentElement) and: [ self isNotVisited: currentElement ])
			ifTrue: [ queue nextPutAll: currentElement dependencies.
					 alreadyVisited add: currentElement ] ].
	^ currentElement
]

{ #category : #accessing }
MTProjectBFSIterator >> project: anMTProject [
	project := anMTProject.
	self reset
]

{ #category : #protocol }
MTProjectBFSIterator >> reset [
	self initialize.
	queue nextPut: project
]
