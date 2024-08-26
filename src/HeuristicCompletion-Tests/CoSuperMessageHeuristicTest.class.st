Class {
	#name : 'CoSuperMessageHeuristicTest',
	#superclass : 'TestCase',
	#category : 'HeuristicCompletion-Tests-Heuristics',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Heuristics'
}

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesApplyForMethodNodeInContextWithNilclass [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: RBMethodNode new
		inContext: (CoCompletionContext new
			completionClass: nil;
			yourself))
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesApplyForMethodNodeInContextWithSuperclass [

	self assert: (CoSuperMessageHeuristic new
		appliesForNode: RBMethodNode new
		inContext: (CoCompletionContext new
			completionClass: (CoMockClass new superclass: CoMockClass new);
			yourself))
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesApplyForSuperMessageSendNodes [

	self assert: (CoSuperMessageHeuristic new
		appliesForNode: (RBMessageNode receiver: (ASTVariableNode superNode variable: SuperVariable new) selector: #foo)
		inContext: nil)
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesNotApplyForMethodNodeInContextWithNoSuperclass [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: RBMethodNode new
		inContext: (CoCompletionContext new
			completionClass: (CoMockClass new superclass: nil);
			yourself))
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesNotApplyForNonSuperMessageSendNodes [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: (RBMessageNode receiver: (ASTVariableNode new variable: (GlobalVariable new)) selector: #foo)
		inContext: nil)
]
