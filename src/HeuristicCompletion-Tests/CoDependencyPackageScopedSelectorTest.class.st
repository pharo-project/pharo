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
    heuristic := CoDependencyMessageHeuristics new.
    node := CoDependencyPackageScopedSelectorTestNodeStub
                messageWithReceiver: CoDependencyPackageScopedSelectorTestReceiverStub variable.

    self assert: (heuristic appliesForNode: node inContext: nil).
]

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testAppliesForMethodNode [
    | heuristic node |
    heuristic := CoDependencyMessageHeuristics new.
    node := CoDependencyPackageScopedSelectorTestNodeStub methodNode.

    self assert: (heuristic appliesForNode: node inContext: nil).
]

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testAppliesForNonVariableReceiverMessage [
    | heuristic node |
    heuristic := CoDependencyMessageHeuristics new.
    node := CoDependencyPackageScopedSelectorTestNodeStub
                messageWithReceiver: CoDependencyPackageScopedSelectorTestReceiverStub new.

    self deny: (heuristic appliesForNode: node inContext: nil).
]

{ #category : 'tests' }
CoDependencyPackageScopedSelectorTest >> testBuildFetcherUsesPackageScopedSelectorFetcher [

	| heuristic node context |
	heuristic := CoDependencyMessageHeuristics new.
	node := Object new.
	context := CoDependencyPackageScopedSelectorTestContextStub new.
	context completionClass: Object.

	fetcher := heuristic buildFetcherFor: node inContext: context.

	self assert: fetcher class equals: CoDependencySelectorFetcher.
	self assert: (fetcher instVarNamed: #completionClass) equals: Object.
	self assert: (fetcher instVarNamed: #astNode) equals: node
]

{ #category : 'tests - dependency' }
CoDependencyPackageScopedSelectorTest >> testLocalFirstThenDependent [

	| localFetcher fetchedElements fetchedSymbols |
	localFetcher := CoDependencySelectorFetcher new.
	localFetcher
		completionClass: ClassWithDependencyOnData2;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'define').

	fetchedElements := localFetcher next: 3.

	self assert: fetchedElements size equals: 3.
	fetchedSymbols := (fetchedElements collect: [:each | each contents]) asArray.
	
	self
		assert: (fetchedSymbols includes: #defineADependencyToAnalyserData1).
	self
		assert: (fetchedSymbols includes: #defineADependencyToDifferentAnalyserData1).
	self
		assert: (fetchedSymbols includes: #defineInDependentOfClassWithDependencyOnData2Package).
		
	
]

{ #category : 'tests - dependency' }
CoDependencyPackageScopedSelectorTest >> testWhenNoLocalThenDependentPackages [

	| localFetcher fetchedElements fetchedSymbols |
	localFetcher := CoDependencySelectorFetcher new.
	localFetcher
		completionClass: ClassWithDependencyOnData2;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'zz').

	fetchedElements := localFetcher next: 3.
	fetchedSymbols := fetchedElements collect: #contents.

	self assert: fetchedElements size equals: 1.
	self assert: (fetchedSymbols includes: #zznotLocallyDefinedInClassWithDependencyOnData2Package)
	"note that we use this zz starting method because Kernel is part of the dependency because of the Object inheritance so this makes the tests more complex
	to write."
]
