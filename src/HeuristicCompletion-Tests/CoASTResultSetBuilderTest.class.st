Class {
	#name : #CoASTResultSetBuilderTest,
	#superclass : #CoBasicCompletionTest,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #tests }
CoASTResultSetBuilderTest >> newCompletionContext [

	^ CoCompletionContext new
		engine: CoMockEngine new;
		yourself
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildArrayHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBArrayNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #array
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildAssignmentHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBAssignmentNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #assignment
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildBlockHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBBlockNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #block
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildCascadeHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBCascadeNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #cascade
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildLiteralArrayHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBLiteralArrayNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #literalArray
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildLiteralHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBLiteralValueNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #literal
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildMessageHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBMessageNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #message
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildMethodHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBMethodNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #method
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildParseErrorHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBParseErrorNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #error
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildPragmaHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBPragmaNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #pragma
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildReturnHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBReturnNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #return
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildSequenceHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBSequenceNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #sequence
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildThisContextHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBVariableNode thisContextNode;
		buildCompletion.
		
	self assert: builder heuristic equals: #thisContext
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableClassVariableHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: ((RBVariableNode named: 'ClassVar') variable: (ClassVariable named: 'ClassVar'));
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableGlobalHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: ((RBVariableNode named: 'Object') variable: Object binding);
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: RBVariableNode new;
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableInstanceVariableHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: ((RBVariableNode named: 'ivar') variable: (Slot named: 'ivar'));
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableTempVariableHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: ((RBVariableNode named: 'temp') variable: (TemporaryVariable named: 'temp'));
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]

{ #category : #tests }
CoASTResultSetBuilderTest >> testBuildVariableUndeclaredHeuristic [

	| builder |
	builder := CoMockASTResultSetBuilder new.
	builder
		completionContext: self newCompletionContext;
		node: ((RBVariableNode named: 'ClassVar') variable: (UndeclaredVariable named: 'ClassVar'));
		buildCompletion.
		
	self assert: builder heuristic equals: #variable
]
