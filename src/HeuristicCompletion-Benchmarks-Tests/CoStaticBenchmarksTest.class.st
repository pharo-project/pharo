Class {
	#name : 'CoStaticBenchmarksTest',
	#superclass : 'TestCase',
	#instVars : [
		'benchmarks'
	],
	#category : 'HeuristicCompletion-Benchmarks-Tests',
	#package : 'HeuristicCompletion-Benchmarks-Tests'
}

{ #category : 'running' }
CoStaticBenchmarksTest >> setUp [ 

	super setUp.
	benchmarks := CoStaticBenchmarks new.
	benchmarks builder: CoGlobalSorterResultSetBuilder new.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> tearDown [ 

	benchmarks := nil.
	super tearDown
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testAccuracyForCompletionIndex [

	"Simulate: the correct method is found at rank=1, prefix=2, exactly once."
    "completionBenchs is a nested dictionary: completionBenchs at: rank => at: prefix => #( count . setOfSelectors )"
    
    "For rank=1, prefix=2, we add {1 . #(someSelector)}"
    benchmarks completionBenchs 
        at: 1
        put: (Dictionary new
                at: 2 put: #(1 (someSelector));
                yourself).
    
    "So totalEntriesPerPrefixSize: 2 => 1. 
     #accuracyForCompletionIndex: (1 to: 1) => #('1st' rank).
     This means we count how many times it was found at rank=1 / total attempts => 1/1 = 1.0."
    
    self assert: (benchmarks accuracyForCompletionIndex: (1 to: 1) withPrefixSize: 2) equals: 1.0.
    
    "Check zero if we ask for rank=2..2"
    self assert: (benchmarks accuracyForCompletionIndex: (2 to: 2) withPrefixSize: 2) equals: 0.0.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testAccuracyForCompletionIndexCalculation [

    "Simulate the following data:
    - Rank=1, Prefix=2, Count=2 (two successful completions at rank 1 with prefix 2)
    - Rank=2, Prefix=2, Count=1 (one successful completion at rank 2 with prefix 2)
    
    Total entries for prefix=2 => 3 (2 + 1)."

    | prefixDict1 prefixDict2 result |
    
    prefixDict1 := Dictionary new.
    prefixDict1 at: 2 put: { 2 . #(selA selB) }. "Rank 1, Prefix 2, Count=2"
    
    prefixDict2 := Dictionary new.
    prefixDict2 at: 2 put: { 1 . #(selC) }. "Rank 2, Prefix 2, Count=1"
    
    benchmarks completionBenchs at: 1 put: prefixDict1. "Simulate rank 1 completions"
    benchmarks completionBenchs at: 2 put: prefixDict2. "Simulate rank 2 completions"
    
    "Calculate accuracy for rank range 1 to 1 (rank 1 only) with prefix size 2.
    Expected: 2/3 = 0.6667."
    result := benchmarks accuracyForCompletionIndex: (1 to: 1) withPrefixSize: 2.
    self assert: result equals: 2/3.
    
    "Calculate accuracy for rank range 1 to 2 (ranks 1 and 2 combined) with prefix size 2.
    Expected: (2 + 1)/3 = 1.0."
    result := benchmarks accuracyForCompletionIndex: (1 to: 2) withPrefixSize: 2.
    self assert: result equals: 1.0.
    
    "Calculate accuracy for rank range 3 to 3 (rank 3 only) with prefix size 2.
    Expected: 0/3 = 0.0."
    result := benchmarks accuracyForCompletionIndex: (3 to: 3) withPrefixSize: 2.
    self assert: result equals: 0.0.


]

{ #category : 'running' }
CoStaticBenchmarksTest >> testAccuracyPerSelectorLength [

    "We log a correct find at rank=1, prefix=2, for the selector 'abcd' (size=4). 
     Then we verify #accuracyPerSelectorLength: 4 => 1.0."
    | prefixDict |
    prefixDict := Dictionary new.
    prefixDict at: 2 put: { 1 . (Set with: 'abcd') }.
    benchmarks completionBenchs at: 1 put: prefixDict.

    self assert: (benchmarks accuracyPerSelectorLength: 4) equals: 1.0.
    self assert: (benchmarks accuracyPerSelectorLength: 5) equals: 0.0.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testAverageMemoryForPrefix [


	benchmarks logMemory: 1000 forPrefix: 2.
	benchmarks logMemory: 3000 forPrefix: 2.
	
	self assert: (benchmarks averageMemoryForPrefix: 2) equals: 2000.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testAverageTimeForPrefix [

	benchmarks logTime: 10 forPrefix: 3.
	benchmarks logTime: 20 forPrefix: 3.
	
	self assert: (benchmarks averageTimeForPrefix: 3) equals: 15.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testBuilder [

	| newBuilder |
	newBuilder := CoASTHeuristicsResultSetBuilder.
	benchmarks builder: newBuilder.
	self assert: benchmarks builder equals: newBuilder.
	
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testDiff [

    | benchA benchB diffResult |
    benchA := CoStaticBenchmarksMock new.
    benchB := CoStaticBenchmarksMock new.

    diffResult := benchA diff: benchB.

    "Check that diffResult is an array."
    self assert: diffResult isArray.

    "Ensure it contains exactly 3 associations."
    self assert: (diffResult size = 3).

    "Check that each entry is an association (key -> value)."
    diffResult do: [ :entry |
        self assert: entry isAssociation.  "Each entry should be a key -> value pair"
        self assert: entry value isArray.  "The value of the association should be an array"
        self assert: (entry value size = 3).  "Ensure the outer array contains 3 elements"
        
        "Ensure that each sub-array has 5 elements"
        entry value do: [ :subArray |
            self assert: subArray isArray.  "Each sub-array should be an array"
            self assert: (subArray size = 5).  "Each sub-array should have 5 elements"
        ].
    ].

]

{ #category : 'running' }
CoStaticBenchmarksTest >> testInitialize [

	| bench time memory |
	bench := benchmarks completionBenchs.
	time := benchmarks completionTimes.
	memory := benchmarks memoryUsages.
	
	self assert: bench isEmpty.
	self assert: time isEmpty.
	self assert: memory isEmpty.
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testInspectionResults [
	"Just verify it returns something without error; 
     you might parse the returned table or string to ensure correct formatting."

	benchmarks accuracyInspectionResults.
	self assert:
		(benchmarks accuracyInspectionResults isKindOf: SpTablePresenter)
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testLogMemory [

   "memoryUsages at: 2 => #( totalBytes count ) => #( 3072 2 )."
	| stored |
	benchmarks logMemory: 1024 forPrefix: 2.
	benchmarks logMemory: 2048 forPrefix: 2.
	stored := benchmarks memoryUsages at:2 ifAbsent: [ #(0 0) ].

	self assert: stored first equals: 3072.
	self assert: stored second equals: 2.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testLogTime [

    "completionTimes at: 2 => #( totalTime count ) => #( 50 2 )."
	| stored |
	benchmarks logTime: 20 forPrefix: 2.
	benchmarks logTime: 30 forPrefix: 2.
	stored := benchmarks completionTimes at: 2 ifAbsent: [ #(0 0) ].
	self assert: stored first equals: 50.
	self assert: stored second equals: 2.	    
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testMMR [

    "Add data to completionBenchs to simulate correct finds at rank=1 for prefix=2.
	Now total for prefix=2 is 2, prefix=3 is 1 => total=3.
   MRR for prefix=2 => average(1/1 + 1/1), that is 1.0 (since all are rank=1).
	MRR for prefix=3 => average(1/1) => 1.0.
   So #mmr => (3 * 1.0) / 3 => 1.0."
   benchmarks completionBenchs
		at: 1
      put: (Dictionary new 
			at: 2 put: #(2 (selA selB));
         at: 3 put: #(1 (selC));
				yourself).
    
    self assert: (benchmarks mmrForPrefixSize: 2) equals: 1.0.
    self assert: (benchmarks mmrForPrefixSize: 3) equals: 1.0.
    self assert: benchmarks mmr equals: 1.0.
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testNDCGValidCase [
    | ndcgValue |
    "Test with a valid case where relevant items exist."

    "Simulate 2 correct finds at rank=1 for prefix=2 => we should get a high NDCG (near 1.0)."
    benchmarks completionBenchs 
        at: 1 
        put: (Dictionary new at: 2 put: #(2 (selA selB)); yourself).


    ndcgValue := benchmarks ndcgForPrefixSize: 2.

    self assert: ndcgValue >= 0.
    self assert: (ndcgValue >= 1.0 and: [ndcgValue < 2.0]).
    self assert: ndcgValue > 0.9.
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testNDCGWithEmptyEntries [ 

    "Test with an empty completionBenchs dictionary, should return 0."

    | ndcgValue |
    ndcgValue := benchmarks ndcgForPrefixSize: 2.

    self assert: ndcgValue = 0.
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testNDCGWithHighRank [
    | ndcgValue |
    "Test NDCG calculation when rank exceeds the expected range."

    benchmarks completionBenchs 
        at: 20 
        put: (Dictionary new at: 2 put: #(5 (selX selY selZ)); yourself).

    ndcgValue := benchmarks ndcgForPrefixSize: 2.

    self assert: ndcgValue >= 0.
    self assert: ndcgValue <= 1.
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testNDCGWithMultipleRanks [
    | ndcgValue |
    "Test with multiple ranked entries to verify proper discounting."

    benchmarks completionBenchs 
        at: 1 
        put: (Dictionary new at: 2 put: #(2 (selA selB)); yourself);
        at: 2 
        put: (Dictionary new at: 2 put: #(1 (selC)); yourself).

    ndcgValue := benchmarks ndcgForPrefixSize: 2.

    self assert: ndcgValue > 0.
    self assert: (ndcgValue > 2.0 and: [ndcgValue < 3.0]).
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testNDCGWithZeroEntries [

	| ndcgValue |
	"Test when there are entries but no valid counts."
	benchmarks completionBenchs at: 1 put: (Dictionary new
			 at: 2 put: #( 0 #(  ) );
			 yourself).
	ndcgValue := benchmarks ndcgForPrefixSize: 2.
	self assert: ndcgValue equals: 0
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testRankDistributionForPrefixSize [

    "We add 1 correct find at ranks 1, 2, and 3 for prefix=2. 
     Then #rankDistributionForPrefixSize: 2 => #(1 1 1 0 0 0 0 0 0 0)."

    | dist |
    benchmarks completionBenchs
        at: 1
        put: (Dictionary new at: 2 put: #(1 (selA)); yourself).
    benchmarks completionBenchs
        at: 2
        put: (Dictionary new at: 2 put: #(1 (selB)); yourself).
    benchmarks completionBenchs
        at: 3
        put: (Dictionary new at: 2 put: #(1 (selC)); yourself).

    dist := benchmarks rankDistributionForPrefixSize: 2.
    self assert: dist size equals: 10.
    self assert: dist equals: #(1 1 1 0 0 0 0 0 0 0).
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testRecallAtK [

    "Total attempts for prefix=2 => 3, so recall@K=1 => 3/3 => 1.0"
    benchmarks completionBenchs
        at: 1
        put: (Dictionary new
                 at: 2 put: #(3 (someSel someSel2 someSel3));
                 yourself).

    self assert: (benchmarks recallAtK: 1 withPrefixSize: 2) equals: 1.0.
    
    "If we ask for rank=3 => same result, because rank=1 is included."
    self assert: (benchmarks recallAtK: 3 withPrefixSize: 2) equals: 1.0.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testScope [ 

	| fakeClass |
	fakeClass := Object.
	benchmarks scope: fakeClass.
	self assert: benchmarks scope equals: fakeClass.
]

{ #category : 'tests' }
CoStaticBenchmarksTest >> testScore [
	"The #score method is a bit arbitrary; it uses #gradeForPrefixSize, 
     which uses #accuracyForCompletionIndex: (2..8). 
     Here we just ensure it doesn't crash 
     and returns a numeric value in the expected range [0..100]."

	benchmarks score.
	self assert: (benchmarks score between: 0 and: 100)
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testTotalMemoryForPrefix [

	benchmarks logMemory: 1000 forPrefix: 2.
	benchmarks logMemory: 2000 forPrefix: 2.
	
	self assert: (benchmarks totalMemoryForPrefix: 2) equals: 3000.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testTotalTime [

	benchmarks logTime: 10 forPrefix: 2.
	benchmarks logTime: 30 forPrefix: 3.
	
	self assert: benchmarks totalTime equals: 40.
]

{ #category : 'running' }
CoStaticBenchmarksTest >> testTotalTimeForPrefix [

	benchmarks logTime: 10 forPrefix: 3.
	benchmarks logTime: 20 forPrefix: 3.
	
	self assert: (benchmarks totalTimeForPrefix: 3) equals: 30.
]
