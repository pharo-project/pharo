"
This is a mock class to simulate an exmpale of data to test some functionalities in the Benchmarks like `diff` ...
"
Class {
	#name : 'CoStaticBenchmarksMock',
	#superclass : 'CoStaticBenchmarks',
	#category : 'HeuristicCompletion-Benchmarks-Tests',
	#package : 'HeuristicCompletion-Benchmarks-Tests'
}

{ #category : 'benchmarks' }
CoStaticBenchmarksMock >> accuracyForCompletionIndex: completionIndexRange withPrefixSize: prefixSize [
    "Returns a fake accuracy value between 0.0 and 1.0 
     (i.e., 0% to 100%) based purely on the inputs, 
     just to have a stable/deterministic output."
    ^ (completionIndexRange first + prefixSize) / 10.0  "Dummy formula"
]

{ #category : 'benchmarks' }
CoStaticBenchmarksMock >> completionIndexes [
    "Returns dummy completion index ranges.
     For example, the real #diff: method expects a collection of integer ranges."
    ^ #((0) (1) (2 to: 3))  "Just an example set of completion indexes"

]

{ #category : 'benchmarks' }
CoStaticBenchmarksMock >> prefixSizes [
    "Returns dummy prefix sizes."
    ^ #(1 2 3 4)
]
