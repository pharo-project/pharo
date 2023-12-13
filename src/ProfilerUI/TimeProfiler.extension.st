Extension { #name : 'TimeProfiler' }

{ #category : '*ProfilerUI' }
TimeProfiler >> report [

	^ self fullReport
]

{ #category : '*ProfilerUI' }
TimeProfiler >> startProfiling [
	"Created for compatibility with Andreas Profiler"
]

{ #category : '*ProfilerUI' }
TimeProfiler >> stopProfiling [
	"Created for compatibility with AndreasProfiler"
]

{ #category : '*ProfilerUI' }
TimeProfiler >> tallyRoot [
	^ rootMessageTally
]

{ #category : '*ProfilerUI' }
TimeProfiler >> totalTime [
	^ rootMessageTally
		ifNotNil: [rootMessageTally time]
		ifNil: [ self error: 'profiler has not yet been run' ]
]
