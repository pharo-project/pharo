"
I run a series of completion benchmarks over a static scope.
I take all existing callsites in a scope (a class, a package) and run the completion with different prefixes (2 to 8), asking for 10 results.
Then I check if the selector at that callsite is in those elements, and in what position.
I accumulate the results in a table per index in the results and prefix size.

I have an inspector extension to show the results in a table.

Examples:

global := CoStaticBenchmarks new
	scope: (CoBenchmarkPackage on: SpAbstractAdapter package);
	builder: (CoGlobalSorterResultSetBuilder new
		sorterClass: AlphabeticSorter;
		yourself);
	run.

staticHeuristics := CoStaticBenchmarks new
	scope: (CoBenchmarkPackage on: SpAbstractAdapter package);
	builder: CoASTHeuristicsResultSetBuilder new;
	run.
"
Class {
	#name : #CoStaticBenchmarks,
	#superclass : #Object,
	#instVars : [
		'scope',
		'completionBenchs',
		'builder'
	],
	#category : #'HeuristicCompletion-Model-Benchmarks'
}

{ #category : #running }
CoStaticBenchmarks class >> runFor: aClass [ 
	
	^ self new
		scope: aClass;
		run
]

{ #category : #inspector }
CoStaticBenchmarks >> accuracyForCompletionIndex: completionIndexRange withPrefixSize: prefixSize [
	
	| totalEntries |
	totalEntries := self totalEntriesPerPrefixSize: prefixSize.
	totalEntries = 0 ifTrue: [ ^ 0 ].
	
	^ (completionIndexRange sum: [ :index |
		(completionBenchs at: index at: prefixSize ifAbsent: [ {0} ]) first
	]) / totalEntries
]

{ #category : #benchmarks }
CoStaticBenchmarks >> benchCallsite: aRBMessageNode atPosition: aPosition [
	
	| originalSelector |
	originalSelector := aRBMessageNode selector.
	2 to: (aRBMessageNode selector size min: 8) do: [ :index | | prefix completion candidates completionIndex previousResultsPerIndexPerPrefixSize |
	
		"The CoGlobalSorterResultSetBuilder uses the selector to pre-narrow the results"	
		prefix := originalSelector copyFrom: 1 to: index.
		aRBMessageNode selector: prefix.

		completion := builder
			node: aRBMessageNode;
			completionContext: (CoBenchmarkContext new
				callsite: aRBMessageNode;
				position: aPosition;
				yourself);
			buildCompletion.
		
		completion filterWithString: prefix.
		candidates := completion first: 10.

		completionIndex := (candidates collect: [:each | each contents]) indexOf: originalSelector.
		
		previousResultsPerIndexPerPrefixSize := completionBenchs
			at: completionIndex
			at: prefix size
			ifAbsent: [ {0 . Set new } ].
		previousResultsPerIndexPerPrefixSize second add: originalSelector.
		completionBenchs
			at: completionIndex
			at: prefix size
			put: {previousResultsPerIndexPerPrefixSize first + 1 . previousResultsPerIndexPerPrefixSize second }
	].
	aRBMessageNode selector: originalSelector.
]

{ #category : #accessing }
CoStaticBenchmarks >> builder: aCompletionBuilder [
	
	builder := aCompletionBuilder
]

{ #category : #inspector }
CoStaticBenchmarks >> completionIndexes [
	
	^ { 1 to: 1.
	  2 to: 2.
	  3 to: 3.
	  4 to: 10.
	  0 to: 0.
	}
]

{ #category : #scoring }
CoStaticBenchmarks >> gradeForPrefixSize: prefixSize [

	^ self accuracyForCompletionIndex: (2 to: 8) withPrefixSize: prefixSize
]

{ #category : #inspector }
CoStaticBenchmarks >> gtResultsTableIn: composite [
	<gtInspectorPresentationOrder: 1>
	| table |
	table := composite table title: 'Results'.
	"columns"
	table
		column: 'Prefix'
		evaluated: [ :completionIndexRange | | label |
			label := '% '.
			label := label, (completionIndexRange size = 1
				ifTrue: [ 
					{ 'fail' . '1st' . '2nd' . '3rd' } at: completionIndexRange first + 1 ]
				ifFalse: [ completionIndexRange first asString , '-', completionIndexRange last asString ]).
			label ]
		width: 70.
	self prefixSizes do: [ :prefixSize |
		table
			column: prefixSize asString
			evaluated: [ :completionIndexRange | | float | 
				float := self accuracyForCompletionIndex: completionIndexRange withPrefixSize: prefixSize.
				(float * 100) printShowingDecimalPlaces: 2 ]
			width: 70 ].
	"rows"
	table display: [ 
		self completionIndexes
	].
	^ table
]

{ #category : #initialization }
CoStaticBenchmarks >> initialize [

	super initialize.
	completionBenchs := Dictionary new
]

{ #category : #inspector }
CoStaticBenchmarks >> prefixSizes [

	^ 2 to: 8
]

{ #category : #printing }
CoStaticBenchmarks >> printOn: aStream [

	aStream
		print: builder;
		nextPutAll: '(';
		print: scope;
		nextPutAll: ')'
]

{ #category : #running }
CoStaticBenchmarks >> run [
	
	scope methodsDo: [ :method |
		method parseTree nodesDo: [ :node |
			node isMessage ifTrue: [ 
				self benchCallsite: node atPosition: node keywordsIntervals first ]
		]
	]
]

{ #category : #accessing }
CoStaticBenchmarks >> scope: aClass [ 
	scope := aClass
]

{ #category : #scoring }
CoStaticBenchmarks >> score [
	
	^ ((1 to: 7) sum: [ :i | (self gradeForPrefixSize: i + 1) / i ]) * 100 / ((1 to: 7) sum: [ :index | 1/index ])
]

{ #category : #inspector }
CoStaticBenchmarks >> totalEntriesPerPrefixSize: aPrefixSize [

	^ completionBenchs sum: [ :benchsPerPrefix | (benchsPerPrefix at: aPrefixSize ifAbsent: [ {0} ]) first ]
]
