private - growing
privateAt: key put: value 
	size == keys size ifTrue: [self growKeysAndValues].
	size := size + 1.
	keys at: size put: key.
	^values at: size put: value