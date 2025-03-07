"
`CoStaticBenchmarksVariables` is a subclass of `CoStaticBenchmarks`  focuses on benchmarking variable name (Class Name) completions. It measures the effectiveness of completion algorithms in predicting variable names based on given prefixes. The class includes methods for extracting variable name prefixes, fetching top-ranked completion candidates, and tracking execution performance through various metrics.""


## Typical Usage:


### First way (We recommand to use this one :):


```smalltalk
global := CoStaticBenchmarksVariables runAllOnClass: CompletionSorter package.

global := CoStaticBenchmarksVariables runAllOnPackage: 'NECompletion'.

""To add the which templates of heurisitcs you want to use""

global := CoStaticBenchmarksVariables runAllOnPackage: 'NECompletion' heuristics: [ :b | CoStaticBenchmarksVariables defaultHeuristics: b ].

global := CoStaticBenchmarksVariables runAllOnPackage: 'NECompletion' heuristics: [ :b | CoStaticBenchmarksVariables customHeuristics: b ].

```

### Second way:


```smalltalk
global := CoStaticBenchmarksVariables new
    scope: (CoBenchmarkPackage on: CompletionSorter package);
    builder: (CoGlobalSorterResultSetBuilder new
        sorterClass: AlphabeticSorter;
        yourself);
    run.

""Get the total accuracy of finding the right result among the first 3 results when typing 2 characters.""
(global accuracyForCompletionIndex: (1 to: 3) withPrefixSize: 2) asFloat.

""To see how well completions do on 5-character selectors""

global := accuracyPerSelectorLength: 5

```
### Second way:

```smalltalk
staticHeuristics := CoStaticBenchmarksVariables new
    scope: (CoBenchmarkPackage on: SpAbstractAdapter package);
    builder: CoASTHeuristicsResultSetBuilder new;
    run.

""Get the total accuracy of finding the right result among the first 3 when typing 2 characters.""
(staticHeuristics accuracyForCompletionIndex: (1 to: 3) withPrefixSize: 2) asFloat.
```


## Extracting the tables into Latex:

```smalltalk
latex := (global at: #SizeSorter) latexResultsFor: #mmr.
```

## Comparing the tables 

```smalltalk
benchA := CoStaticBenchmarksVariables new
    scope: (CoBenchmarkPackage on: CompletionSorter package);
    builder: (CoGlobalSorterResultSetBuilder new
        sorterClass: AlphabeticSorter;
        yourself);
    run.


benchB := CoStaticBenchmarksVariables new
    scope: (CoBenchmarkPackage on: SpAbstractValidation package);
    builder: (CoGlobalSorterResultSetBuilder new
        sorterClass: AlphabeticSorter;
        yourself);
    run.


comparison := benchA diff: benchB.
```
"
Class {
	#name : 'CoStaticBenchmarksVariables',
	#superclass : 'CoStaticBenchmarks',
	#category : 'HeuristicCompletion-Benchmarks',
	#package : 'HeuristicCompletion-Benchmarks'
}

{ #category : 'running' }
CoStaticBenchmarksVariables class >> customHeuristics: aBuilder [

" For now `CoPackageScopedGlobalVariableFetcher` is NOT implemented, there will be another PR where `CoPackageScopedGlobalVariableFetcher` is implement, then you can use this template.
	aBuilder variablesHeuristic: (aBuilder newHeuristicBuilder
			 add: CoLocalScopeVariablesHeuristic new;
			 add: CoWorkspaceVariablesHeuristic new;
			 add: CoInstanceAccessibleVariablesHeuristic new;
			 add: (CoGlobalVariablesHeuristic new globalVariableFetcherClass:
						  CoPackageScopedGlobalVariableFetcher);
			 build)
"
]

{ #category : 'running' }
CoStaticBenchmarksVariables class >> defaultHeuristics: aBuilder [

	aBuilder variablesHeuristic: (aBuilder newHeuristicBuilder
			 add: CoLocalScopeVariablesHeuristic new;
			 add: CoWorkspaceVariablesHeuristic new;
			 add: CoInstanceAccessibleVariablesHeuristic new;
			 add: CoGlobalVariablesHeuristic new;
			 build)
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> benchCallsite: aMessageNode atPosition: aPosition [

	| receiver |
	receiver := aMessageNode receiver.

	2 to: (self maxPrefixSizeFor: receiver) do: [ :index |
		| prefix startTime startMemory completion candidates completionIndex |
		startTime := self startTimeMeasurement.
		startMemory := self startMemoryMeasurement.

		prefix := self extractPrefixFrom: receiver at: index.
		aMessageNode receiver: (OCVariableNode named: prefix).

		completion := self
			              buildCompletionFor: aMessageNode
			              atPosition: aPosition.

		candidates := self
			              fetchTopCandidatesFrom: completion
			              usingPrefix: prefix.
		completionIndex := self
			                   findCompletionIndexFor: receiver
			                   inCandidates: candidates.

		self
			trackCompletionResultsFor: receiver
			atIndex: completionIndex
			withPrefix: prefix.

		self logMemoryUsageSince: startMemory forPrefixSize: prefix size.
		self logExecutionTimeSince: startTime forPrefixSize: prefix size ].

	aMessageNode receiver: receiver
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> buildCompletionFor: aMessageNode atPosition: aPosition [

	^ builder
		  node: aMessageNode receiver;
		  completionContext: (CoBenchmarkContext new
				   callsite: aMessageNode;
				   position: aPosition;
				   yourself);
		  buildCompletion
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> extractPrefixFrom: receiver at: index [

	^ receiver name copyFrom: 1 to: index
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> fetchTopCandidatesFrom: completion usingPrefix: prefix [

	completion replaceFilterWith:
		(CoCaseSensitiveBeginsWithFilter filterString: prefix).
	^ completion first: 10
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> findCompletionIndexFor: receiver inCandidates: candidates [

	^ (candidates collect: [ :each | each contents ]) indexOf:
		  receiver name
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> logExecutionTimeSince: startTime forPrefixSize: prefixSize [

	| executionTime |
	executionTime := Time millisecondClockValue - startTime.
	self logTime: executionTime forPrefix: prefixSize
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> logMemoryUsageSince: startMemory forPrefixSize: prefixSize [

	| memoryUsed |
	memoryUsed := self currentMemoryUsage - startMemory.
	self logMemory: memoryUsed forPrefix: prefixSize
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> maxPrefixSizeFor: receiver [

	^ receiver name size min: 8
]

{ #category : 'running' }
CoStaticBenchmarksVariables >> run [

	scope methodsDo: [ :method |
		method parseTree nodesDo: [ :node |
			(node isMessage and: [
				 node receiver isVariable and: [
					 node receiver name first isUppercase ] ]) ifTrue: [
				self benchCallsite: node atPosition: node keywordsIntervals first ] ] ]
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> startMemoryMeasurement [

	^ self currentMemoryUsage
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> startTimeMeasurement [

	^ Time millisecondClockValue
]

{ #category : 'benchmarks' }
CoStaticBenchmarksVariables >> trackCompletionResultsFor: receiver atIndex: completionIndex withPrefix: prefix [

	| previousResultsPerIndexPerPrefixSize |
	previousResultsPerIndexPerPrefixSize := completionBenchs
		                                        at: completionIndex
		                                        at: prefix size
		                                        ifAbsent: [
			                                        {
				                                        0.
				                                        Set new } ].
	previousResultsPerIndexPerPrefixSize second add: receiver.
	completionBenchs at: completionIndex at: prefix size put: {
			(previousResultsPerIndexPerPrefixSize first + 1).
			previousResultsPerIndexPerPrefixSize second }
]
