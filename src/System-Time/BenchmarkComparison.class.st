"
I am the BenchmarkComparison class, responsible for comparing two BenchmarkResult instances and providing insight into their relative performance.

My instances can be created using `BlockClosure>>#benchCompareTo:`, which takes another block as an argument.

I provide the #frequencyFactor and #periodFactor methods to access and analyze the comparison factors.
"
Class {
	#name : 'BenchmarkComparison',
	#superclass : 'Object',
	#instVars : [
		'referenceResult',
		'comparisonResult'
	],
	#category : 'System-Time',
	#package : 'System-Time'
}

{ #category : 'accessing' }
BenchmarkComparison >> comparisonResult [

	^ comparisonResult
]

{ #category : 'accessing' }
BenchmarkComparison >> comparisonResult: aBenchmarkResult [

	comparisonResult := aBenchmarkResult
]

{ #category : 'comparing' }
BenchmarkComparison >> frequencyFactor [

	^ comparisonResult frequency / referenceResult frequency
]

{ #category : 'testing' }
BenchmarkComparison >> isEmpty [

	^ (referenceResult isNil | comparisonResult isNil or: [
		   referenceResult isEmpty or: [ comparisonResult isEmpty ] ]) not
]

{ #category : 'comparing' }
BenchmarkComparison >> periodFactor [

	^ comparisonResult period / referenceResult period
]

{ #category : 'printing' }
BenchmarkComparison >> printOn: stream [

	self isEmpty
		ifFalse: [ stream << 'empty' ]
		ifTrue: [
			referenceResult frequency printOn: stream showingDecimalPlaces: 3.
			stream << '/s * '.
			self frequencyFactor printOn: stream showingDecimalPlaces: 3.
			stream << ' = '.
			comparisonResult frequency printOn: stream showingDecimalPlaces: 3.
			stream << '/s' ]
]

{ #category : 'accessing' }
BenchmarkComparison >> referenceResult [

	^ referenceResult
]

{ #category : 'accessing' }
BenchmarkComparison >> referenceResult: aBenchmarkResult [

	referenceResult := aBenchmarkResult
]
