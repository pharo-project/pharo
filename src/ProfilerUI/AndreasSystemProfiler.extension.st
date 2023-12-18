Extension { #name : 'AndreasSystemProfiler' }

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> runBlock: aBlock [
	"Compatibility with TimeProfiler"

	^ aBlock value
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> tallyRoot [
	^ tallyRoot
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> totalTally [
	^ totalTally
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> totalTime [
	^ totalTime
]
