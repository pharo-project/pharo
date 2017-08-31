acccessing
managers
	^ managers ifNil: [ managers := WeakKeyDictionary new ]