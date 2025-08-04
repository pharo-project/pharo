"
I'm an abstract superclass for defining AST Based Complishons
"
Class {
	#name : 'CoASTResultSetBuilder',
	#superclass : 'CoResultSetBuilder',
	#instVars : [
		'astNode',
		'node'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'testing' }
CoASTResultSetBuilder class >> isAbstract [

	^ self == CoASTResultSetBuilder
]

{ #category : 'accessing' }
CoASTResultSetBuilder >> astNode [

	^ astNode
]

{ #category : 'API - building' }
CoASTResultSetBuilder >> buildCompletion [

	"Visiting the node configures the completion for that node"
	astNode := self parseNode.
	^ astNode acceptVisitor: self
]

{ #category : 'API - building' }
CoASTResultSetBuilder >> configureEmptyFetcherForNode: aNode [

	^ self
		configureFetcher: CoEmptyFetcher new
		forNode: aNode
]

{ #category : 'private - building' }
CoASTResultSetBuilder >> configureFetcher: aFetcher forNode: aNode [

	| completion |
	completion := CoResultSet fetcher: aFetcher.
	completion filter: (CoBeginsWithFilter caseSensitive: NECPreferences caseSensitive filterString: completionContext completionToken).
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: completionContext completionToken).
	^ completion
]

{ #category : 'private - building' }
CoASTResultSetBuilder >> configureFetcherForNode: aNode usingHeuristicAvoidingRepetitions: heuristic [

	| fetcher |
	fetcher := heuristic fetcherFor: aNode inContext: completionContext.
	^ self
		configureFetcher: fetcher withoutRepetition withNarrowHistory
		forNode: aNode
]

{ #category : 'accessing' }
CoASTResultSetBuilder >> node: aNode [

	node := aNode
]

{ #category : 'API - building' }
CoASTResultSetBuilder >> parseNode [

	| n |
	^ node ifNil: [
		n := completionContext completionClass compiler
			source: completionContext source;
			isScripting: completionContext isScripting;
			parse.
		OCTypingVisitor new visit: n.
		(n nodeBeforeOffset: completionContext position) ifNil: [ n ] ]
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitAnnotationMarkNode: aAnnotationMarkNode [

	^ self visitValueNode: aAnnotationMarkNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitArgumentVariableNode: anArgumentNode [

	^ self visitVariableNode: anArgumentNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitArrayNode: anArrayNode [

	^ self visitValueNode: anArrayNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitAssignmentNode: aAssignmentNode [

	^ self visitValueNode: aAssignmentNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitBlockNode: aBlockNode [

	^ self visitValueNode: aBlockNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitCascadeNode: aCascadeNode [

	^ self visitValueNode: aCascadeNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitClassVariableNode: aVariableNode [

	^ self visitVariableNode: aVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitCommentNode: aComment [ 

	^ self visitNode: aComment
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitEnglobingErrorNode: aVariableNode [
	^ self visitNode: aVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitGlobalVariableNode: aVariableNode [

	^ self visitVariableNode: aVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitInstanceVariableNode: aVariableNode [
	^ self visitVariableNode: aVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitLiteralArrayNode: aLiteralArrayNode [

	^ self visitValueNode: aLiteralArrayNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitLiteralValueNode: aLiteralValueNode [

	^ self visitValueNode: aLiteralValueNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitLiteralVariableNode: aVariableNode [
	^ self visitVariableNode: aVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitMessageNode: aMessageNode [

	^ self visitValueNode: aMessageNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitMethodNode: aMethodNode [

	^ self visitNode: aMethodNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitNode: aNode [

	^ self configureEmptyFetcherForNode: aNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitParseErrorNode: aParseErrorNode [

	^ self visitNode: aParseErrorNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitPragmaNode: aPragmaNode [

	^ self visitNode: aPragmaNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitReturnNode: aReturnNode [

	^ self visitNode: aReturnNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitSelfNode: aSelfNode [

	^ self visitValueNode: aSelfNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitSequenceNode: aSequenceNode [

	^ self visitNode: aSequenceNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitSuperNode: aSuperNode [

	^ self visitValueNode: aSuperNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitTemporaryVariableNode: anArgumentNode [

	^ self visitVariableNode: anArgumentNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitThisContextNode: aThisContextNode [

	^ self visitValueNode: aThisContextNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitValueNode: aVariableNode [

	^ self visitNode: aVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitVariableNode: aVariableNode [

	^ self visitValueNode: aVariableNode
]
