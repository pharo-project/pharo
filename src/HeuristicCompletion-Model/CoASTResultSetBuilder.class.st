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
		ASTTypingVisitor new visit: n.
		(n nodeBeforeOffset: completionContext position) ifNil: [ n ] ]
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitAnnotationMarkNode: aRBAnnotationMarkNode [

	^ self visitValueNode: aRBAnnotationMarkNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitArgumentVariableNode: anArgumentNode [

	^ self visitVariableNode: anArgumentNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitArrayNode: aRBArrayNode [

	^ self visitValueNode: aRBArrayNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitAssignmentNode: aRBAssignmentNode [

	^ self visitValueNode: aRBAssignmentNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitBlockNode: aRBBlockNode [

	^ self visitValueNode: aRBBlockNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitCascadeNode: aRBCascadeNode [

	^ self visitValueNode: aRBCascadeNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitClassVariableNode: aRBVariableNode [

	^ self visitVariableNode: aRBVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitCommentNode: aRBComment [ 

	^ self visitNode: aRBComment
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitEnglobingErrorNode: aRBVariableNode [
	^ self visitNode: aRBVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitGlobalVariableNode: aRBVariableNode [

	^ self visitVariableNode: aRBVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitInstanceVariableNode: aRBVariableNode [
	^ self visitVariableNode: aRBVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitLiteralArrayNode: aLiteralArrayNode [

	^ self visitValueNode: aLiteralArrayNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitLiteralValueNode: aRBLiteralValueNode [

	^ self visitValueNode: aRBLiteralValueNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitLiteralVariableNode: aRBVariableNode [
	^ self visitVariableNode: aRBVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitMessageNode: aRBMessageNode [

	^ self visitValueNode: aRBMessageNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitMethodNode: aRBMethodNode [

	^ self visitNode: aRBMethodNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitNode: aNode [

	^ self configureEmptyFetcherForNode: aNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitParseErrorNode: aRBParseErrorNode [

	^ self visitNode: aRBParseErrorNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitPragmaNode: aRBPragmaNode [

	^ self visitNode: aRBPragmaNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitReturnNode: aRBReturnNode [

	^ self visitNode: aRBReturnNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitSelfNode: aRBSelfNode [

	^ self visitValueNode: aRBSelfNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitSequenceNode: aRBSequenceNode [

	^ self visitNode: aRBSequenceNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitSuperNode: aRBSuperNode [

	^ self visitValueNode: aRBSuperNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitTemporaryVariableNode: anArgumentNode [

	^ self visitVariableNode: anArgumentNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitThisContextNode: aRBThisContextNode [

	^ self visitValueNode: aRBThisContextNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitValueNode: aRBVariableNode [

	^ self visitNode: aRBVariableNode
]

{ #category : 'visiting' }
CoASTResultSetBuilder >> visitVariableNode: aRBVariableNode [

	^ self visitValueNode: aRBVariableNode
]
