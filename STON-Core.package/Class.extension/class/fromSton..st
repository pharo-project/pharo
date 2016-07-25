*ston-core
fromSton: stonReader
	| theClassName theClass |
	theClassName := stonReader parseListSingleton.
	theClass := self environment at: theClassName.
	^ theClass