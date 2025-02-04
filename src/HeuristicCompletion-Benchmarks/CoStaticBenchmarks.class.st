"
`CoStaticBenchmarks`, is designed to run completion benchmarks over a given **scope** (typically a class or package). It explores **completion suggestions** by trying out various prefix sizes (from 2 to 8 characters) and recording:

1. **Accuracy** (whether the correct selector is found among the top 10 suggestions and at which index).
2. **Timing** (the total and average time taken to compute suggestions for each prefix size).

Below is a short overview of the main responsibilities and methods:

## Main Responsibilities

1. **Setup and Running (`run`, `runFor:`)**
   - `run` iterates through all methods in the given scope, parsing them and finding message callsites to benchmark.
   - `runFor:` is a convenience class method to instantiate and immediately run benchmarks for a given class.

2. **Benchmarking a Callsite (`benchCallsite:atPosition:`)**
   - For each callsite, it loops from a prefix length of 2 to 8 characters (or fewer if the selector is shorter).
   - It records whether the original selector was found among the top 10 completion candidates and at which index.
   - It also tracks the time it took to produce the suggestions, accumulating them in `completionTimes`.

3. **Measuring Accuracy (`accuracyForCompletionIndex:withPrefixSize:`)**
   - Determines how often the correct selector is found within specific top-index ranges (e.g., 1st, 2nd, 3rd) for a given prefix size.
   - This is computed by dividing the number of successful finds by the total number of attempts.

4. **Measuring Time (`logTime:forPrefix:`, `totalTimeForPrefix:`)**
   - Each benchmark run logs the execution time in `completionTimes`.
   - Methods such as `totalTimeForPrefix:` and `averageTimeForPrefix:` allow for aggregating and presenting these timings.

5. **Inspection and Presentation (`inspectionResults`, `timeResults`)**
   - Includes methods decorated with `<inspectorPresentationOrder:>` to present **accuracy results** in a table (via `SpTablePresenter`).
   - Presents **timing results** in a similar way, showing total and average execution times per prefix size.

## Key Data Structures

- **`completionBenchs`**: A nested dictionary storing the count of how many times a selector was found at a certain prefix size and position (index in the top-10 list).
- **`completionTimes`**: A dictionary storing the total and count of all execution times, keyed by prefix size.

## Typical Usage

Here is an example of how you might run benchmarks on a given package or class:

```smalltalk
global := CoStaticBenchmarks new
    scope: (CoBenchmarkPackage on: SpAbstractAdapter package);
    builder: (CoGlobalSorterResultSetBuilder new
        sorterClass: AlphabeticSorter;
        yourself);
    run.
```

Get the total accuracy of finding the right result among the first 3 results when typing 2 characters.""""
``` smalltalk
(global accuracyForCompletionIndex: (1 to: 3) withPrefixSize: 2) asFloat.
```

And similarly:

```smalltalk
staticHeuristics := CoStaticBenchmarks new
    scope: (CoBenchmarkPackage on: SpAbstractAdapter package);
    builder: CoASTHeuristicsResultSetBuilder new;
    run.
```

Get the total accuracy of finding the right result among the first 3 when typing 2 characters.
```smalltalk
(staticHeuristics accuracyForCompletionIndex: (1 to: 3) withPrefixSize: 2) asFloat.
```

To show all the Benchmarks use:

```smalltalk
global := CoStaticBenchmarks runAllOnPackage: SpAbstractAdapter package 
```

Using these benchmarks, you can measure:

- **Which prefix sizes produce the best completion accuracy.**
- **How quickly the suggestions are generated** (i.e., performance considerations).
- **Overall completion performance** for different sets of heuristics or sorters.

"
Class {
	#name : 'CoStaticBenchmarks',
	#superclass : 'Object',
	#instVars : [
		'scope',
		'completionBenchs',
		'builder',
		'completionTimes'
	],
	#category : 'HeuristicCompletion-Benchmarks',
	#package : 'HeuristicCompletion-Benchmarks'
}

{ #category : 'running' }
CoStaticBenchmarks class >> runAllOnPackage: aPackage [

    "Runs the CoStaticBenchmarks for every sorter class in sorterClasses, all using the given package as the scope. 
    Answers a Dictionary from sorter class symbols to the resulting CoStaticBenchmarks instance so you can inspect or retrieve results later."
    | results |
    results := Dictionary new.
    
    self sorterClasses do: [ :sorterSymbol |
        | sorterClass benchmark |
        
        "Retrieve the actual class object for the symbol"
        sorterClass := Smalltalk at: sorterSymbol ifAbsent: [ 
            self error: (String streamContents: [ :s | 
    				s nextPutAll: 'Sorter class '.
    				s nextPutAll: sorterSymbol asString.
    				s nextPutAll: ' not found in Smalltalk image.'
			])
        ].

        "Create and run a new benchmark for that sorter"
        benchmark := self new
            scope: (CoBenchmarkPackage on: aPackage);
            builder: (CoGlobalSorterResultSetBuilder new
                sorterClass: sorterClass;
                yourself);
            yourself.

        benchmark run.

        "Store the instance so we can retrieve/inspect it later"
        results at: sorterSymbol put: benchmark
    ].

    ^ results
]

{ #category : 'running' }
CoStaticBenchmarks class >> runFor: aClass [

	^ self new
		scope: aClass;
		run
]

{ #category : 'running' }
CoStaticBenchmarks class >> sorterClasses [

    "Answers an array of the symbols of all sorter classes to be run."
    ^ #( AlphabeticSorter ReverseAlphabeticSorter NoSorter SizeSorter )
]

{ #category : 'inspector' }
CoStaticBenchmarks >> accuracyForCompletionIndex: completionIndexRange withPrefixSize: prefixSize [

	| totalEntries |
	totalEntries := self totalEntriesPerPrefixSize: prefixSize.
	totalEntries = 0 ifTrue: [ ^ 0 ].

	^ (completionIndexRange sum: [ :index |
		(completionBenchs at: index at: prefixSize ifAbsent: [ {0} ]) first
	]) / totalEntries
]

{ #category : 'inspector' }
CoStaticBenchmarks >> averageTimeForPrefix: prefixSize [

    | times |
    times := completionTimes at: prefixSize ifAbsent: [ #( 0 0 ) ].
    times second = 0
        ifTrue: [ ^ 0 ]
        ifFalse: [ ^ times first / times second ]
]

{ #category : 'benchmarks' }
CoStaticBenchmarks >> benchCallsite: aMessageNode atPosition: aPosition [
 
    | originalSelector |
    originalSelector := aMessageNode selector.

    2 to: (aMessageNode selector size min: 8) do: [ :index | 
        | prefix startTime endTime executionTime 
          completion candidates completionIndex 
          previousResultsPerIndexPerPrefixSize |

        "Start timing"
        startTime := Time millisecondClockValue.

        prefix := originalSelector copyFrom: 1 to: index.
        aMessageNode selector: prefix.

        completion := builder
            node: aMessageNode;
            completionContext: (CoBenchmarkContext new
                callsite: aMessageNode;
                position: aPosition;
                yourself);
            buildCompletion.

        completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: prefix).
        candidates := completion first: 10.

        completionIndex := (candidates collect: [ :each | each contents ]) indexOf: originalSelector.

        "Existing logic: update completionBenchs dictionary"
        previousResultsPerIndexPerPrefixSize := completionBenchs
            at: completionIndex
            at: prefix size
            ifAbsent: [ { 0 . Set new } ].
        previousResultsPerIndexPerPrefixSize second add: originalSelector.
        completionBenchs
            at: completionIndex
            at: prefix size
            put: { previousResultsPerIndexPerPrefixSize first + 1 . previousResultsPerIndexPerPrefixSize second }.

        "Stop timing and record it"
        endTime := Time millisecondClockValue.
        executionTime := endTime - startTime.
        self logTime: executionTime forPrefix: prefix size
    ].

    "Restore the original selector"
    aMessageNode selector: originalSelector
]

{ #category : 'accessing' }
CoStaticBenchmarks >> builder: aCompletionBuilder [

	builder := aCompletionBuilder
]

{ #category : 'inspector' }
CoStaticBenchmarks >> completionIndexes [

	^ { 1 to: 1.
	  2 to: 2.
	  3 to: 3.
	  4 to: 10.
	  0 to: 0.
	}
]

{ #category : 'scoring' }
CoStaticBenchmarks >> gradeForPrefixSize: prefixSize [

	^ self accuracyForCompletionIndex: (2 to: 8) withPrefixSize: prefixSize
]

{ #category : 'initialization' }
CoStaticBenchmarks >> initialize [

	super initialize.
	completionBenchs := Dictionary new.
	builder := CoASTHeuristicsResultSetBuilder new.
	completionTimes := Dictionary new. 
]

{ #category : 'inspector' }
CoStaticBenchmarks >> inspectionResults [

	<inspectorPresentationOrder: 0 title: 'Results'>
	| table |
	table := SpTablePresenter new
		         items: self completionIndexes;
		         addColumn: (SpCompositeTableColumn new
				          title: 'Prefix';
				          addColumn:
					          (SpStringTableColumn evaluated: [ :completionIndexRange |
							           | label |
							           label := '% '.
							           label := label , (completionIndexRange size = 1
									                     ifTrue: [
										                     { 'fail'. '1st'. '2nd'. '3rd' } at:
												                     completionIndexRange first + 1 ]
									                     ifFalse: [
										                     completionIndexRange first asString
										                     , '-'
										                     , completionIndexRange last asString ]).
							           label ]);
				          yourself).
	self prefixSizes do: [ :prefixSize |
		table addColumn: (SpStringTableColumn
				 title: prefixSize asString
				 evaluated: [ :completionIndexRange |
					 | float |
					 float := self
						          accuracyForCompletionIndex: completionIndexRange
						          withPrefixSize: prefixSize.
					 float * 100 printShowingDecimalPlaces: 2 ]) ].
	^ table
]

{ #category : 'inspector' }
CoStaticBenchmarks >> logTime: executionTime forPrefix: prefixSize [

    | times |
    times := completionTimes
        at: prefixSize
        ifAbsent: [ #( 0 0 )  ].  "Default is { totalTime = 0 . count = 0 }"
    completionTimes
        at: prefixSize
        put: { times first + executionTime.  times second + 1 }.
]

{ #category : 'inspector' }
CoStaticBenchmarks >> prefixSizes [

	^ 2 to: 8
]

{ #category : 'printing' }
CoStaticBenchmarks >> printOn: aStream [

	aStream
		print: builder;
		nextPutAll: '(';
		print: scope;
		nextPutAll: ')'
]

{ #category : 'running' }
CoStaticBenchmarks >> run [

	scope methodsDo: [ :method |
		method parseTree nodesDo: [ :node |
			node isMessage ifTrue: [
				self benchCallsite: node atPosition: node keywordsIntervals first ]
		]
	]
]

{ #category : 'accessing' }
CoStaticBenchmarks >> scope: aClass [

	scope := aClass
]

{ #category : 'scoring' }
CoStaticBenchmarks >> score [

	^ ((1 to: 7) sum: [ :i | (self gradeForPrefixSize: i + 1) / i ]) * 100 / ((1 to: 7) sum: [ :index | 1/index ])
]

{ #category : 'running' }
CoStaticBenchmarks >> timeResults [

    <inspectorPresentationOrder: 1 title: 'Time Results'>
    | table |
    table := SpTablePresenter new
        items: self prefixSizes;  "2..8"
        addColumn: (
            SpStringTableColumn
                title: 'Prefix'
                evaluated: [ :prefixSize | prefixSize asString ]
        );
        addColumn: (
            SpStringTableColumn
                title: 'Total (ms)'
                evaluated: [ :prefixSize | (self totalTimeForPrefix: prefixSize) asString ]
        );
		  addColumn: (
            SpStringTableColumn
                title: 'Count'
                evaluated: [ :prefixSize |
                    (completionTimes at: prefixSize ifAbsent: [ #( 0 0 ) ]) second asString ]
        );
        addColumn: (
            SpStringTableColumn
                title: 'Average (ms)'
                evaluated: [ :prefixSize | 
                    (self averageTimeForPrefix: prefixSize) 
                       asFloat round:2;
                        asString 
                ]
        );
        yourself.

    ^ table
]

{ #category : 'inspector' }
CoStaticBenchmarks >> totalEntriesPerPrefixSize: aPrefixSize [

	^ completionBenchs sum: [ :benchsPerPrefix | (benchsPerPrefix at: aPrefixSize ifAbsent: [ {0} ]) first ]
]

{ #category : 'running' }
CoStaticBenchmarks >> totalTime [

    "Sum over all prefix sizes"
    ^ completionTimes values
        inject: 0
        into: [ :sum :timeArray | sum + timeArray first ]
]

{ #category : 'running' }
CoStaticBenchmarks >> totalTimeForPrefix: prefixSize [

    | times |
    times := completionTimes at: prefixSize ifAbsent: [ #( 0 0 ) ].
    ^ times first  "the accumulated total time in ms"
]

{ #category : 'running' }
CoStaticBenchmarks >> totalTimeSummary [

	<inspectorPresentationOrder: 2 title: 'Total Time (ms)'>
	^ self totalTime asPresenter
]
