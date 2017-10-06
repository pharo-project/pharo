examples
timingMany
	| dicts keys size loops count |
	
	size := 10.
	count := 10000.
	loops := 10.
	
	keys := (1 to: size) collect:[:n| 'foo',n asString].
	
	dicts := OrderedCollection new.
	count timesRepeat:[ | dict |
		dict := WeakKeyDictionary new.
		keys do:[:k| dict at: k put: k].
		dicts add: dict.
	].
	Smalltalk garbageCollect.
	^Time millisecondsToRun: [
			loops timesRepeat:[
				dicts do: [:d| d finalizeValues].
			]].
