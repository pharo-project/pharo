Extension { #name : #Date }

{ #category : #'*ston-core' }
Date class >> fromSton: stonReader [
	^ self readFrom: stonReader parseListSingleton readStream
]

{ #category : #'*ston-core' }
Date >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
Date >> stonOn: stonWriter [
	"Use an ISO style YYYY-MM-DD representation"
	
	stonWriter writeObject: self listSingleton: self yyyymmdd
]
