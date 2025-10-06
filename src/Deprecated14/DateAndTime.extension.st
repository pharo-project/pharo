Extension { #name : 'DateAndTime' }

{ #category : '*Deprecated14' }
DateAndTime class >> clock [
	"the provider of real time seconds/milliseconds."

	self deprecated: 'Use Time instead.' transformWith: '`@rcv clock' -> 'Time'.

	^ Time
]

{ #category : '*Deprecated14' }
DateAndTime class >> millisecondClockValue [

	self deprecated: 'Use Time as a receiver instead.' transformWith: '`@rcv millisecondClockValue' -> 'Time millisecondClockValue'.
	^ Time millisecondClockValue
]

{ #category : '*Deprecated14' }
DateAndTime class >> totalSeconds [
	"Answer the total seconds ellapsed since the epoch: 1 January 1901"

	self deprecated: 'Use Time as a receiver instead.' transformWith: '`@rcv totalSeconds' -> 'Time totalSeconds'.

	^ Time totalSeconds
]
