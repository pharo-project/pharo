Class {
	#name : 'ClassWithDependencyOnData2',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Analyser-Data4',
	#package : 'HeuristicCompletion-Analyser-Data4'
}

{ #category : 'methods for tests' }
ClassWithDependencyOnData2 >> defineADependencyToAnalyserData1 [

	
	^ AnalyserData1
]

{ #category : 'methods for tests' }
ClassWithDependencyOnData2 >> defineADependencyToDifferentAnalyserData1 [

	^ DifferentAnalyserData1
]
