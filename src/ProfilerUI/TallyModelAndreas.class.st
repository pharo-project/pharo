"
I am an adapter for tallies from AndreasProfiler
"
Class {
	#name : 'TallyModelAndreas',
	#superclass : 'TallyModel',
	#category : 'ProfilerUI-Model',
	#package : 'ProfilerUI',
	#tag : 'Model'
}

{ #category : 'accessing' }
TallyModelAndreas >> childrenItems [

	^ super childrenItems reversed
]

{ #category : 'accessing' }
TallyModelAndreas >> methodOrBlock [
	^ item methodOrBlock
]
