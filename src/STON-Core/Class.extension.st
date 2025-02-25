Extension { #name : 'Class' }

{ #category : '*STON-Core' }
Class class >> fromSton: stonReader [
	| theClassName theClass |
	theClassName := stonReader parseListSingleton.
	theClass := self environment at: theClassName.
	^ theClass
]

{ #category : '*STON-Core' }
Class >> stonOn: stonWriter [
	stonWriter
		writeObject: self
		listSingleton: self name asSymbol
]
