"
I am the an AST based heuristics builder.

When building the complishon, I build the AST from the source in the context, and get the current node from the context position.
Then I do a double dispatch on the current node and from it
1) I decide what fetchers to create
2) I extract the currently typed text and set is as initial filter
"
Class {
	#name : 'CoASTHeuristicsResultSetBuilder',
	#superclass : 'CoASTResultSetBuilder',
	#instVars : [
		'messageHeuristic',
		'methodNodeHeuristic',
		'variablesHeuristic',
		'symbolsHeuristic'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'deprecated' }
CoASTHeuristicsResultSetBuilder >> messageHeuristic [
	
	self deprecated: 'Use messagesHeuristic' transformWith: '`@rec messageHeuristic' -> '`@rec messagesHeuristic'.
	^ self messagesHeuristic
]

{ #category : 'deprecated' }
CoASTHeuristicsResultSetBuilder >> messageHeuristic: aHeuristic [

		self deprecated: 'Use messagesHeuristic' transformWith: '`@rec messageHeuristic: `@arg' -> '`@rec messagesHeuristic: `@arg'.
	self messagesHeuristic: aHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> messagesHeuristic [

	^ messageHeuristic ifNil: [
			  messageHeuristic := self newHeuristicBuilder
				                     
				                      add: CoSelfMessageHeuristic new;
				                      add: CoSuperMessageHeuristic new;
				                      add: CoDependencyMessageHeuristics new;
				                      add: CoTypedReceiverMessageHeuristic new;
				                      add: CoInitializeInferencedMessageHeuristic new;
				                      add: CoLiteralMessageHeuristic new;
				                      add: CoGlobalVariableMessageHeuristic new;
				                      add: CoVariableWithTypeNameMessageHeuristic new;
				                      add: CoVariableValueMessageHeuristic new;
				                      add: CoUnknownMessageHeuristic new;
				                      "add: CoDependencySelectorFetcher new;"build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> messagesHeuristic: aHeuristic [

	messageHeuristic := aHeuristic
]

{ #category : 'deprecated' }
CoASTHeuristicsResultSetBuilder >> methodNodeHeuristic [

		self deprecated: 'Use methodNodeHeuristic' transformWith: '`@rec methodNodeHeuristic' -> '`@rec methodNodesHeuristic'.
		^ self methodNodesHeuristic
]

{ #category : 'deprecated' }
CoASTHeuristicsResultSetBuilder >> methodNodeHeuristic: aHeuristic [

	self deprecated: 'Use methodNodesHeuristic:' transformWith: '`@rec methodNodeHeuristic: `@arg' -> '`@rec methodNodesHeuristic: `@arg'.
	self methodNodesHeuristic: aHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> methodNodesHeuristic [

	^ methodNodeHeuristic ifNil: [
			  methodNodeHeuristic := self newHeuristicBuilder
				                         add: CoSuperMessageHeuristic new;
				                         add: CoUnknownMessageHeuristic new;
				                         build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> methodNodesHeuristic: aHeuristic [

	methodNodeHeuristic := aHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> newHeuristicBuilder [
	^ CoHeuristicBuilder new
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> symbolsHeuristic [
	^ symbolsHeuristic ifNil: [ symbolsHeuristic := self newHeuristicBuilder
		add: CoLiteralSymbolHeuristic new;
		build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> symbolsHeuristic: aHeuristics [

	 symbolsHeuristic := aHeuristics
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> variablesHeuristic [

	^ variablesHeuristic ifNil: [ variablesHeuristic := self newHeuristicBuilder
		add: CoLocalScopeVariablesHeuristic new;
		add: CoWorkspaceVariablesHeuristic new;
		add: CoInstanceAccessibleVariablesHeuristic new;
		add: (CoGlobalVariablesHeuristic new globalVariableFetcherClass:
					CoDependencyGlobalVariableFetcher);
			add: CoGlobalVariablesHeuristic new;
		build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> variablesHeuristic: aHeuristic [

	variablesHeuristic := aHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitLiteralValueNode: aNode [

	completionContext completionToken ifEmpty: [ ^ super visitLiteralValueNode: aNode ].

	^ self
		configureFetcherForNode: aNode
		usingHeuristicAvoidingRepetitions: self symbolsHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitMessageNode: aMessageNode [

	completionContext completionToken ifEmpty: [ ^ super visitMessageNode: aMessageNode ].

	^ self configureFetcherForNode: aMessageNode usingHeuristicAvoidingRepetitions: self messagesHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitMethodNode: aMethodNode [

	completionContext completionToken ifEmpty: [ ^ super visitMethodNode: aMethodNode ].

	^ self configureFetcherForNode: aMethodNode usingHeuristicAvoidingRepetitions: self methodNodesHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitValueNode: aNode [

	| offset |
	offset := completionContext completionTokenStart.
	completionContext completionToken isEmpty ifTrue: [
		offset > aNode stopWithoutParentheses ifTrue: [
			| aMessageNode |
			"We are after a value node with a empty completion token.
			So, try to complete a message send from scratch with the value node as the receiver.
			In order to be compatible with the current complex completion framework,
			we just inject a synthetic message node in the AST with an empty selector.
			This empty selector will then be completed according to the existing rules and heuristics.
			It is a little hackish but we believe in magic."
			aMessageNode := OCMessageNode new.
			aNode replaceWith: aMessageNode.
			aMessageNode receiver: aNode.
			aMessageNode selector: #''.
			^ self
				  configureFetcherForNode: aMessageNode
				  usingHeuristicAvoidingRepetitions: self messagesHeuristic ] ].

	^ super visitValueNode: aNode
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitVariableNode: aVariableNode [

	completionContext completionToken ifEmpty: [ ^ super visitVariableNode: aVariableNode ].

	^ self
		configureFetcherForNode: aVariableNode
		usingHeuristicAvoidingRepetitions: self variablesHeuristic
]
