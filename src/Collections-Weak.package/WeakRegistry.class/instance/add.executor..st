adding
add: anObject executor: anExecutor
" with new finalization, we can simply override associations with key == nil in valueDictionary "
	self protected: [ | finItem |
		finItem := valueDictionary at: anObject ifAbsent: [
			WeakFinalizationList hasNewFinalization ifTrue: [
				valueDictionary overridingAt: anObject put: (WeakFinalizerItem new list: list object: anObject) ]
			ifFalse: [
				valueDictionary at: anObject put: (WeakFinalizerItem new list: list object: anObject) 
				]
		].
		finItem add: anExecutor ].
	^ anObject
