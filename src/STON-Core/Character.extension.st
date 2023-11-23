Extension { #name : 'Character' }

{ #category : '*STON-Core' }
Character class >> fromSton: stonReader [
	^ stonReader parseListSingleton first
]

{ #category : '*STON-Core' }
Character >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString
]
