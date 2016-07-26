actions
flushAllCaches
	| beforeBytes afterBytes beforeVersions afterVersions |
	Cursor wait showWhile: [
		beforeBytes := Smalltalk garbageCollect.
		beforeVersions := MCVersion allSubInstances size.
		MCFileBasedRepository flushAllCaches.
		afterBytes := Smalltalk garbageCollect.
		afterVersions := MCVersion allSubInstances size.
	].
	^self inform: (beforeVersions - afterVersions) asString, ' versions flushed', String cr,
 		(afterBytes - beforeBytes) asStringWithCommas, ' bytes reclaimed'