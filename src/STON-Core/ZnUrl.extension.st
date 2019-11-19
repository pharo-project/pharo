Extension { #name : #ZnUrl }

{ #category : #'*ston-core' }
ZnUrl class >> fromSton: stonReader [
	^ self fromString: stonReader parseListSingleton 
]

{ #category : #'*ston-core' }
ZnUrl >> stonContainSubObjects [
	^ false
]

{ #category : #'*ston-core' }
ZnUrl class >> stonName [
	^ #URL
]

{ #category : #'*ston-core' }
ZnUrl >> stonOn: stonWriter [
	stonWriter writeObject: self listSingleton: self asString 
]
