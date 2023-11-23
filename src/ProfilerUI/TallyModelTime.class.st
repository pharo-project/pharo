"
I am an adapter for tallies from TimeProfiler
"
Class {
	#name : 'TallyModelTime',
	#superclass : 'TallyModel',
	#category : 'ProfilerUI-Model',
	#package : 'ProfilerUI',
	#tag : 'Model'
}

{ #category : 'accessing' }
TallyModelTime >> methodName [

	^ item method methodClass name, '>>' , item method selector asString
]
