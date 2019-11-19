Extension { #name : #ByteArray }

{ #category : #'*ston-core' }
ByteArray class >> fromSton: stonReader [
	"Use a hex representation"
	
	^ self readHexFrom: stonReader parseListSingleton
]

{ #category : #'*ston-core' }
ByteArray >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
ByteArray >> stonOn: stonWriter [
	"Use a hex representation"
	
	stonWriter writeObject: self listSingleton: self hex
]
