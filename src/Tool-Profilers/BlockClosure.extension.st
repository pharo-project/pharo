Extension { #name : #BlockClosure }

{ #category : #'*Tool-Profilers' }
BlockClosure >> timeProfile [
	^Smalltalk tools timeProfiler onBlock: self
]

{ #category : #'*Tool-Profilers' }
BlockClosure >> timeToRunWithoutGC [
	"Answer the number of milliseconds taken to execute this block without GC time."
 
	^ Smalltalk vm totalGCTime + self timeToRun asMilliSeconds -  Smalltalk vm totalGCTime

]
