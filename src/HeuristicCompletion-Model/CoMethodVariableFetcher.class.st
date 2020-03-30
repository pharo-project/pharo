Class {
	#name : #CoMethodVariableFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionASTNode'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #accessing }
CoMethodVariableFetcher >> completionASTNode [
	^ completionASTNode
]

{ #category : #accessing }
CoMethodVariableFetcher >> completionASTNode: anObject [
	completionASTNode := anObject
]

{ #category : #enumerating }
CoMethodVariableFetcher >> entriesDo: aBlock [

	self methodVariableNamesDo: [ :each |
		aBlock value: (NECLocalEntry contents: each node: self) ]
]

{ #category : #accessing }
CoMethodVariableFetcher >> methodVariableNamesDo: aBlock [

	| currentNode |
	currentNode := completionASTNode.
	[ currentNode notNil ] whileTrue: [ 
		"ASTs are strange.
		There is no way to ask the variables defined in the scope in an ordered way.
		Hack our way through"
		self flag: #notNice.
		currentNode isSequence ifTrue: [ 
			currentNode temporaryNames do: aBlock.
			currentNode parent argumentNames do: aBlock ].
		currentNode := currentNode parent ]
]
