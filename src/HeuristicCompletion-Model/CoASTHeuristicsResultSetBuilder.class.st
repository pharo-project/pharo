"
I am the an AST based heuristics builder.

When building the complishon, I build the AST from the source in the context, and get the current node from the context position.
Then I do a double dispatch on the current node and from it
1) I decide what fetchers to create
2) I extract the currently typed text and set is as initial filter
"
Class {
	#name : #CoASTHeuristicsResultSetBuilder,
	#superclass : #CoASTResultSetBuilder,
	#instVars : [
		'messageHeuristic',
		'methodNodeHeuristic',
		'variablesHeuristic'
	],
	#category : #'HeuristicCompletion-Model-SystemIntegration'
}

{ #category : #heuristics }
CoASTHeuristicsResultSetBuilder >> messageHeuristic [

	^ messageHeuristic ifNil: [ messageHeuristic := self newHeuristicBuilder
		add: CoSelfMessageHeuristic new;
		add: CoSuperMessageHeuristic new;
		add: CoInitializeInferencedMessageHeuristic new;
		add: CoLiteralMessageHeuristic new;
		add: CoGlobalVariableMessageHeuristic new;
		add: CoVariableWithTypeNameMessageHeuristic new;
		add: CoVariableValueMessageHeuristic new;
		add: CoUnknownMessageHeuristic new;
		build ]
]

{ #category : #heuristics }
CoASTHeuristicsResultSetBuilder >> methodNodeHeuristic [
	^ methodNodeHeuristic ifNil: [
		methodNodeHeuristic := self newHeuristicBuilder
		add: CoSuperMessageHeuristic new;
		add: CoUnknownMessageHeuristic new;
		build ]
]

{ #category : #heuristics }
CoASTHeuristicsResultSetBuilder >> newHeuristicBuilder [
	^ CoHeuristicBuilder new
]

{ #category : #heuristics }
CoASTHeuristicsResultSetBuilder >> variablesHeuristic [
	^ variablesHeuristic ifNil: [ variablesHeuristic := self newHeuristicBuilder
		add: CoLocalScopeVariablesHeuristic new;
		add: CoWorkspaceVariablesHeuristic new;
		add: CoInstanceAccessibleVariablesHeuristic new;
		add: CoGlobalVariablesHeuristic new;
		build ]
]

{ #category : #visiting }
CoASTHeuristicsResultSetBuilder >> visitMessageNode: aRBMessageNode [ 
	
	^ self
		configureFetcherForNode: aRBMessageNode
		usingHeuristicAvoidingRepetitions: self messageHeuristic
]

{ #category : #visiting }
CoASTHeuristicsResultSetBuilder >> visitMethodNode: aRBMethodNode [ 

	^ self
		configureFetcherForNode: aRBMethodNode
		usingHeuristicAvoidingRepetitions: self methodNodeHeuristic
]

{ #category : #visiting }
CoASTHeuristicsResultSetBuilder >> visitVariableNode: aRBVariableNode [ 

	^ self
		configureFetcherForNode: aRBVariableNode
		usingHeuristicAvoidingRepetitions: self variablesHeuristic
]
