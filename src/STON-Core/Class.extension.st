Extension { #name : #Class }

{ #category : #'*ston-core' }
Class class >> fromSton: stonReader [
	| theClassName theClass |
	theClassName := stonReader parseListSingleton.
	theClass := self environment at: theClassName.
	^ theClass
]

{ #category : #'*ston-core' }
Class >> stonOn: stonWriter [
	stonWriter 
		writeObject: self 
		listSingleton: self name asSymbol
]
