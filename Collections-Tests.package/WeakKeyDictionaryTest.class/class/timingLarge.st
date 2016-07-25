as yet unclassified
timingLarge
	| dict keys size loops |
	
	size := 100000.
	loops := 10.
	
	keys := (1 to: size) collect:[:n| 'foo',n asString].
	Smalltalk garbageCollect.
	dict := WeakKeyDictionary new.
	keys do:[:k| dict at: k put: k].
	Smalltalk garbageCollect.
	^Time millisecondsToRun: [
		loops timesRepeat:[
			dict at: 'bar','0' put: 100.
			Smalltalk garbageCollectMost.
			dict finalizeValues.
		]].
