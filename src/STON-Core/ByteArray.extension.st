Extension { #name : 'ByteArray' }

{ #category : '*STON-Core' }
ByteArray class >> fromSton: stonReader [
	"Use a hex representation"

	^ self readHexFrom: stonReader parseListSingleton
]

{ #category : '*STON-Core' }
ByteArray >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
ByteArray >> stonOn: stonWriter [
	"Use a hex representation"

	stonWriter writeObject: self listSingleton: self hex
]
