process list
processNameList
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally := CPUWatcher ifNotNil: [ CPUWatcher current ifNotNil: [ CPUWatcher current tally ] ].
	^ (processList asOrderedCollection copyWithout: nil)
		collect: [ :each | 
			| percent |
			percent := tally
				ifNotNil: [ (((tally occurrencesOf: each) * 100.0 / tally size roundTo: 1) asString padLeftTo: 2) , '% ' ]
				ifNil: [ '' ].
			percent , (self prettyNameForProcess: each) ]