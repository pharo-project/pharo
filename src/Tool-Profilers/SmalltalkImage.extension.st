Extension { #name : #SmalltalkImage }

{ #category : #'*Tool-Profilers' }
SmalltalkImage >> highResClock [	"Smalltalk highResClock"
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register."
	<primitive: 'primitiveHighResClock'>
	^0
]

{ #category : #'*Tool-Profilers' }
SmalltalkImage >> profilePrimitive [
	"Primitive. Answer the primitive sample by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfilePrimitive'>
	^self primitiveFailed
]

{ #category : #'*Tool-Profilers' }
SmalltalkImage >> profileSample [
	"Primitive. Answer the last sample taken by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSample'>
	^self primitiveFailed
]

{ #category : #'*Tool-Profilers' }
SmalltalkImage >> profileSemaphore: aSemaphore [
	"Primitive. Install the semaphore to be used for profiling, 
	or nil if no semaphore should be used.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSemaphore'>
	^self primitiveFailed
]

{ #category : #'*Tool-Profilers' }
SmalltalkImage >> profileStart: counter [
	"Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results).
	The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore.
	If the argument is less or equal to zero, it disables profiling."
	<primitive: 'primitiveProfileStart'>
	^self primitiveFailed
]

{ #category : #'*Tool-Profilers' }
SmalltalkImage >> reportCPUandRAM [
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	SmalltalkImage current reportCPUandRAM
	"	

	"VM statistics (Memory use and GC, mainly)"
	'Log-MemoryStats.txt' asFileReference writeStreamDo: [ :aStream | 
		aStream nextPutAll: self vm statisticsReport ].
	
	"Process list"
	'Log-ProcessList.txt' asFileReference writeStreamDo: [ :aStream |
		self tools processBrowser new processNameList do: [ :each | 
			aStream nextPutAll: each; cr ] ].

"Fork all these, so they run in sequence, as the system is back running"
[
	"Process taking most CPU"
	'Log-ThePig.txt' asFileReference writeStreamDo: [ :aStream |
		self tools processBrowser dumpPigStackOn: aStream ].
	
	"Tally of all processes"
	'Log-FullTally.txt' asFileReference writeStreamDo: [ :aStream |
		| tally |
		tally := MessageTally new.
		tally reportOtherProcesses: true.
		tally spyAllEvery: 1 on: [ (Delay forMilliseconds: 1000) wait ].
		tally report: aStream ].

	"Memory Analysis"
	'Log-MemoryAnalysis.txt' asFileReference writeStreamDo: [ :aStream |
		SpaceTally new printSpaceAnalysis: 1 on: aStream ]

] forkNamed: 'CPU usage analysis'
]
