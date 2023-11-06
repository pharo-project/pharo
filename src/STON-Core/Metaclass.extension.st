Extension { #name : 'Metaclass' }

{ #category : '*STON-Core' }
Metaclass class >> fromSton: stonReader [
	| theClassName theClass theMetaclass |
	theClassName := stonReader parseListSingleton.
	theClass := self environment at: theClassName.
	theMetaclass := theClass class.
	^ theMetaclass
]

{ #category : '*STON-Core' }
Metaclass >> stonName [
	^ #Class
]

{ #category : '*STON-Core' }
Metaclass >> stonOn: stonWriter [
	stonWriter
		writeObject: self
		listSingleton: self instanceSide name asSymbol
]
