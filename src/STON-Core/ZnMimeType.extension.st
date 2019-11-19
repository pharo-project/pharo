Extension { #name : #ZnMimeType }

{ #category : #'*ston-core' }
ZnMimeType class >> fromSton: stonReader [
	^ self fromString: stonReader parseListSingleton
]

{ #category : #'*ston-core' }
ZnMimeType >> stonContainSubObjects [
	^ false
]

{ #category : #'*ston-core' }
ZnMimeType class >> stonName [
	^ #MimeType
]

{ #category : #'*ston-core' }
ZnMimeType >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString 
]
