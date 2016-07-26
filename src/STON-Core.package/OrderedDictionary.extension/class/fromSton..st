*ston-core
fromSton: stonReader
	"My instances are stored as maps."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary