*ston-core
fromSton: stonReader
	"Instances of STON mapClass will be read directly and won't arrive here.
	Other (sub)classes will use this method."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary