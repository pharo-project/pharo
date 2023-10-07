Extension { #name : 'DateAndTime' }

{ #category : '*STON-Core' }
DateAndTime class >> fromSton: stonReader [
	^ self readFrom: stonReader parseListSingleton readStream
]

{ #category : '*STON-Core' }
DateAndTime >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
DateAndTime >> stonOn: stonWriter [
	"Use an ISO representation with all details YYYY-MM-DDTHH:MM:SS.N+TZ (with optional nanoseconds and timezone offset)"

	stonWriter writeObject: self listSingleton:
		(String streamContents: [ :stream |
			self printOn: stream withLeadingSpace: false ])
]
