finalization
retryWithGC: execBlock until: testBlock
	"Retry execBlock as long as testBlock returns false. Do an incremental GC after the first try, a full GC after the second try."
	| blockValue |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.