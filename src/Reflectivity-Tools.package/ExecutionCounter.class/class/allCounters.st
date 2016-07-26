accessing
allCounters
	^ Counters ifNil: [ Counters := WeakKeyDictionary new ]