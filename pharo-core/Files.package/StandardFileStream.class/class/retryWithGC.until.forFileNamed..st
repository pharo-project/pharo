registry
retryWithGC: execBlock until: testBlock forFileNamed: fullName
	"Re-implemented to only force GC if a file with the given name exists"
	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	"See if we have a file with the given name"
	foundIt := self registry keys "hold on strongly for now" 
		anySatisfy:[:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.