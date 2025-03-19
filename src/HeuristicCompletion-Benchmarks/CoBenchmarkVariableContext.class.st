Class {
	#name : 'CoBenchmarkVariableContext',
	#superclass : 'CoBenchmarkContext',
	#category : 'HeuristicCompletion-Benchmarks',
	#package : 'HeuristicCompletion-Benchmarks'
}

{ #category : 'accessing' }
CoBenchmarkVariableContext >> completionToken [

	^ callsite name
]
