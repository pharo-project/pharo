Extension { #name : 'ZnMimeType' }

{ #category : '*STON-Extensions' }
ZnMimeType class >> fromSton: stonReader [
	^ self fromString: stonReader parseListSingleton
]

{ #category : '*STON-Extensions' }
ZnMimeType >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Extensions' }
ZnMimeType class >> stonName [
	^ #MimeType
]

{ #category : '*STON-Extensions' }
ZnMimeType >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString
]
