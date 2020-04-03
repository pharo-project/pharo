Class {
	#name : #CoASTResultSetBuilderTest,
	#superclass : #CoBasicCompletionTest,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildArrayHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBArrayNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #array
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildAssignmentHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBAssignmentNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #assignment
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildBlockHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBBlockNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #block
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildCascadeHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBCascadeNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #cascade
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildLiteralArrayHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBLiteralArrayNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #literalArray
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildLiteralHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBLiteralValueNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #literal
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildMessageHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBMessageNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #message
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildMethodHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBMethodNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #method
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildParseErrorHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBParseErrorNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #error
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildReturnHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBReturnNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #return
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildSequenceHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBSequenceNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #sequence
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: CoCompletionContext new;
		node: RBVariableNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]
