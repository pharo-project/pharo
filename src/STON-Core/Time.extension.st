Extension { #name : 'Time' }

{ #category : '*STON-Core' }
Time class >> fromSton: stonReader [
	^ self readFrom: stonReader parseListSingleton readStream
]

{ #category : '*STON-Core' }
Time >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
Time >> stonOn: stonWriter [
	"Use an ISO style HH:MM:SS.N representation (with optional nanoseconds)"

	stonWriter writeObject: self listSingleton:
		(String streamContents: [ :stream |
			self print24: true showSeconds: true on: stream ])
]
