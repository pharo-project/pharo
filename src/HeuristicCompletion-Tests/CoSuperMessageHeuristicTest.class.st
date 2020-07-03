Class {
	#name : #CoSuperMessageHeuristicTest,
	#superclass : #TestCase,
	#category : #'HeuristicCompletion-Tests-Heuristics'
}

{ #category : #test }
CoSuperMessageHeuristicTest >> testDoesApplyForMethodNodeInContextWithNilclass [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: RBMethodNode new
		inContext: (CoCompletionContext new
			completionClass: nil;
			yourself))
]

{ #category : #test }
CoSuperMessageHeuristicTest >> testDoesApplyForMethodNodeInContextWithSuperclass [

	self assert: (CoSuperMessageHeuristic new
		appliesForNode: RBMethodNode new
		inContext: (CoCompletionContext new
			completionClass: (CoMockClass new superclass: CoMockClass new);
			yourself))
]

{ #category : #test }
CoSuperMessageHeuristicTest >> testDoesApplyForSuperMessageSendNodes [

	self assert: (CoSuperMessageHeuristic new
		appliesForNode: (RBMessageNode receiver: RBSuperNode new selector: #foo)
		inContext: nil)
]

{ #category : #test }
CoSuperMessageHeuristicTest >> testDoesNotApplyForMethodNodeInContextWithNoSuperclass [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: RBMethodNode new
		inContext: (CoCompletionContext new
			completionClass: (CoMockClass new superclass: nil);
			yourself))
]

{ #category : #test }
CoSuperMessageHeuristicTest >> testDoesNotApplyForNonSuperMessageSendNodes [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: (RBMessageNode receiver: RBVariableNode new selector: #foo)
		inContext: nil)
]
