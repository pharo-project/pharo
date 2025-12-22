Class {
	#name : 'CoDependencyPackageScopedSelectorTest',
	#superclass : 'CoMiniTestCase',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testAppliesForMessageWithVariableReceiver [
    | heuristic node |
    heuristic := CoDependencyMessageHeuritics new.
    node := CoDependencyPackageScopedSelectorTestNodeStub
                messageWithReceiver: CoDependencyPackageScopedSelectorTestReceiverStub variable.

    self assert: (heuristic appliesForNode: node inContext: nil).
]

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testAppliesForMethodNode [
    | heuristic node |
    heuristic := CoDependencyMessageHeuritics new.
    node := CoDependencyPackageScopedSelectorTestNodeStub methodNode.

    self assert: (heuristic appliesForNode: node inContext: nil).
]

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testAppliesForNonVariableReceiverMessage [
    | heuristic node |
    heuristic := CoDependencyMessageHeuritics new.
    node := CoDependencyPackageScopedSelectorTestNodeStub
                messageWithReceiver: CoDependencyPackageScopedSelectorTestReceiverStub new.

    self deny: (heuristic appliesForNode: node inContext: nil).
]

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testBuildFetcherUsesPackageScopedSelectorFetcher [
    | heuristic node context |
    heuristic := CoDependencyMessageHeuritics new.
    node := Object new.
    context := CoDependencyPackageScopedSelectorTestContextStub new.
    context completionClass: Object.

    fetcher := heuristic buildFetcherFor: node inContext: context.

    self assert: fetcher class equals: CoPackageScopedSelectorFetcher.
    self assert: (fetcher instVarNamed: #completionClass) equals: Object.
    self assert: (fetcher instVarNamed: #astNode) equals: node.
]
