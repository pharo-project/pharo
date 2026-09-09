Class {
	#name : 'AnalyserData1',
	#superclass : 'Object',
	#classVars : [
		'Ans'
	],
	#pools : [
		'AnalyserData1SharedPool'
	],
	#category : 'HeuristicCompletion-Analyser-Data',
	#package : 'HeuristicCompletion-Analyser-Data'
}

{ #category : 'accessing' }
AnalyserData1 >> accessingAnalyserData1SharedPool [

	^ AnalyserData1SharedPool
]

{ #category : 'accessing' }
AnalyserData1 >> foo [  

	^ AnalyserA 
]

{ #category : 'accessing' }
AnalyserData1 >> referToAnalyzer1 [

	^ AnalyserData1 foo
]

{ #category : 'accessing' }
AnalyserData1 >> sharedCaller [

	^ Ans foo
]

{ #category : 'accessing' }
AnalyserData1 >> sharedPoolCaller [

	^ Ansp1 foo
]
