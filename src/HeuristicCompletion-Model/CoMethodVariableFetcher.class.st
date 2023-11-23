"
I am a complishon fetcher that autocompletes variable names accessible in the lexical context of a method.
That is, block temporaries, block arguments, method temporaries and method arguments.

From a given ast node, I walk up the tree looking for scopes and yield the variables I find.
"
Class {
	#name : 'CoMethodVariableFetcher',
	#superclass : 'CoFetcher',
	#instVars : [
		'completionASTNode'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoMethodVariableFetcher >> completionASTNode [
	^ completionASTNode
]

{ #category : 'accessing' }
CoMethodVariableFetcher >> completionASTNode: anObject [
	completionASTNode := anObject
]

{ #category : 'enumerating' }
CoMethodVariableFetcher >> entriesDo: aBlock [

	self methodVariableNamesDo: [ :each |
		aBlock value: (NECLocalEntry contents: each node: completionASTNode) ]
]

{ #category : 'accessing' }
CoMethodVariableFetcher >> methodVariableNamesDo: aBlock [

	| currentNode |
	currentNode := completionASTNode.
	[ currentNode isNotNil ] whileTrue: [
		"ASTs are strange.
		There is no way to ask the variables defined in the scope in an ordered way.
		Hack our way through"
		self flag: #notNice.
		currentNode isSequence ifTrue: [
			currentNode temporaryNames do: aBlock.
			currentNode parent argumentNames do: aBlock ].
		currentNode := currentNode parent ]
]
