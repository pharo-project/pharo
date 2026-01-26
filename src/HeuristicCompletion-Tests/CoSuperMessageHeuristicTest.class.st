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
		appliesForNode: OCMethodNode new
		inContext: (CoCompletionContext new
			completionClass: nil;
			yourself))
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesApplyForMethodNodeInContextWithSuperclass [

	self assert: (CoSuperMessageHeuristic new
		appliesForNode: OCMethodNode new
		inContext: (CoCompletionContext new
			completionClass: (CoMockClass new superclass: CoMockClass new);
			yourself))
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesApplyForSuperMessageSendNodes [

	self assert: (CoSuperMessageHeuristic new
		appliesForNode: (OCMessageNode receiver: (OCVariableNode superNode variable: SuperVariable new) selector: #foo)
		inContext: nil)
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesNotApplyForMethodNodeInContextWithNoSuperclass [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: OCMethodNode new
		inContext: (CoCompletionContext new
			completionClass: (CoMockClass new superclass: nil);
			yourself))
]

{ #category : 'tests' }
CoSuperMessageHeuristicTest >> testDoesNotApplyForNonSuperMessageSendNodes [

	self deny: (CoSuperMessageHeuristic new
		appliesForNode: (OCMessageNode receiver: (OCVariableNode new variable: (GlobalVariable new)) selector: #foo)
		inContext: nil)
]
