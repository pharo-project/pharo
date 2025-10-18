Extension { #name : 'BlockClosure' }

{ #category : '*Tool-Profilers' }
BlockClosure >> timeToRunWithoutGC [
	"Answer the number of milliseconds taken to execute this block without GC time."

	^ Smalltalk vm totalGCTime + self millisecondsToRun -  Smalltalk vm totalGCTime
]
