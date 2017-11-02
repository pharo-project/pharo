Extension { #name : #Time }

{ #category : #'*ston-core' }
Time class >> fromSton: stonReader [
	^ self readFrom: stonReader parseListSingleton readStream
]

{ #category : #'*ston-core' }
Time >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
Time >> stonOn: stonWriter [
	"Use an ISO style HH:MM:SS.N representation (with optional nanoseconds)"
	 
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self print24: true showSeconds: true on: stream ])
]
