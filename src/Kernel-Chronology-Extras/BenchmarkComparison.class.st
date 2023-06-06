Class {
	#name : #BenchmarkComparison,
	#superclass : #Object,
	#instVars : [
		'referenceResult',
		'comparisonResult'
	],
	#category : #'Kernel-Chronology-Extras'
}

{ #category : #accessing }
BenchmarkComparison >> comparisonResult [

	^ comparisonResult
]

{ #category : #accessing }
BenchmarkComparison >> comparisonResult: aBenchmarkResult [

	comparisonResult := aBenchmarkResult
]

{ #category : #printing }
BenchmarkComparison >> frequencyFactor [

	^ comparisonResult frequency / referenceResult frequency
]

{ #category : #testing }
BenchmarkComparison >> isEmpty [

	^ (referenceResult isNil | comparisonResult isNil or: [
		   referenceResult isEmpty or: [ comparisonResult isEmpty ] ]) not
]

{ #category : #printing }
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

{ #category : #accessing }
BenchmarkComparison >> referenceResult [

	^ referenceResult
]

{ #category : #accessing }
BenchmarkComparison >> referenceResult: aBenchmarkResult [

	referenceResult := aBenchmarkResult
]
