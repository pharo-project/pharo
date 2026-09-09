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
		'variablesHeuristic',
		'symbolsHeuristic',
		'messagesHeuristic',
		'methodNodesHeuristic'
	],
	#classVars : [
		'MessagesHeuristic',
		'MethodNodesHeuristic',
		'SymbolsHeuristic',
		'VariablesHeuristic'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'class initialization' }
CoASTHeuristicsResultSetBuilder class >> initialize [
	"self initialize"
	
	self initializeMessagesHeuristic.
	self initializeMethodNodeHeuristic.
	self initializeSymbolHeuristic.
	self initializeVariablesHeuristic 

	
]

{ #category : 'class initialization' }
CoASTHeuristicsResultSetBuilder class >> initializeMessagesHeuristic [

		MessagesHeuristic :=  OrderedCollection <- {  
				                      CoSelfMessageHeuristic new .
				                      CoSuperMessageHeuristic new.
				                      CoDependencyMessageHeuristics new .
				                      CoTypedReceiverMessageHeuristic  new .
				                      CoInitializeInferencedMessageHeuristic  new .
				                      CoLiteralMessageHeuristic  new . 
				                      CoGlobalVariableMessageHeuristic new  .
				                      CoVariableWithTypeNameMessageHeuristic  new .
				                      CoVariableValueMessageHeuristic  new .
				                      CoUnknownMessageHeuristic new  }
]

{ #category : 'class initialization' }
CoASTHeuristicsResultSetBuilder class >> initializeMethodNodeHeuristic [

	MethodNodesHeuristic := OrderedCollection <- {
		                       CoSuperMessageHeuristic new.
		                       CoUnknownMessageHeuristic new } 
]

{ #category : 'class initialization' }
CoASTHeuristicsResultSetBuilder class >> initializeSymbolHeuristic [

	SymbolsHeuristic := OrderedCollection <- { CoLiteralSymbolHeuristic new } 
]

{ #category : 'class initialization' }
CoASTHeuristicsResultSetBuilder class >> initializeVariablesHeuristic [

	VariablesHeuristic := OrderedCollection <- {
		                      CoLocalScopeVariablesHeuristic new.
		                      CoWorkspaceVariablesHeuristic new.
		                      CoInstanceAccessibleVariablesHeuristic new.
		                      (CoGlobalVariablesHeuristic new globalVariableFetcherClass: CoDependencyGlobalVariableFetcher).
		                      CoGlobalVariablesHeuristic new } 
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> messagesHeuristic [

	^ MessagesHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> messagesHeuristic: aCollection [

	MessagesHeuristic := aCollection 
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> methodNodesHeuristic [

	^ MethodNodesHeuristic 
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> methodNodesHeuristic: aCollection [

	MethodNodesHeuristic := aCollection 
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> symbolsHeuristic [

	^ SymbolsHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> symbolsHeuristic: aCollection [

	SymbolsHeuristic := aCollection 
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> variablesHeuristic [

	^ VariablesHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder class >> variablesHeuristic: aCollection [

	VariablesHeuristic := aCollection 
]

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

	^ messagesHeuristic ifNil: [
			| builder |
			builder := self newHeuristicBuilder.
			MessagesHeuristic do: [ :aCls | builder add: aCls ].
			messagesHeuristic := builder build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> messagesHeuristic: aHeuristic [

	messagesHeuristic := aHeuristic
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

	^ methodNodesHeuristic ifNil: [
			| builder |
			builder := self newHeuristicBuilder.
			MethodNodesHeuristic do: [ :fetcher | builder add: fetcher ].
			methodNodesHeuristic := builder build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> methodNodesHeuristic: aHeuristic [

	methodNodesHeuristic := aHeuristic
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> newHeuristicBuilder [
	^ CoHeuristicBuilder new
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> symbolsHeuristic [
	^ symbolsHeuristic ifNil: [ 
			| builder |
			builder := self newHeuristicBuilder.
			SymbolsHeuristic do: [ :aCls | builder add: aCls ].
			symbolsHeuristic :=  builder build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> symbolsHeuristic: aHeuristics [

	 symbolsHeuristic := aHeuristics
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> variablesHeuristic [

	^ variablesHeuristic ifNil: [

			| builder |
			builder := self newHeuristicBuilder.
			VariablesHeuristic do: [ :aCls | builder add: aCls ].
			variablesHeuristic :=  builder build ]
]

{ #category : 'heuristics' }
CoASTHeuristicsResultSetBuilder >> variablesHeuristic: aHeuristic [

	variablesHeuristic := aHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitArrayNode: aNode [

	^ self
		configureFetcherForNode: aNode
		usingHeuristicAvoidingRepetitions: self variablesHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitBlockNode: aNode [

	^ self
		configureFetcherForNode: aNode
		usingHeuristicAvoidingRepetitions: self variablesHeuristic
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
CoASTHeuristicsResultSetBuilder >> visitReturnNode: aNode [

	^ self
		configureFetcherForNode: aNode
		usingHeuristicAvoidingRepetitions: self variablesHeuristic
]

{ #category : 'visiting' }
CoASTHeuristicsResultSetBuilder >> visitSequenceNode: aNode [

	^ self
		configureFetcherForNode: aNode
		usingHeuristicAvoidingRepetitions: self variablesHeuristic
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
			aMessageNode selector: (OCSelectorNode value: #'').
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
