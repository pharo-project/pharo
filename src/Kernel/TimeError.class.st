"
I am an error thrown if Time expands outside a 24 hour range.
"
Class {
	#name : #TimeError,
	#superclass : #Error,
	#category : #'Kernel-Chronology'
}

{ #category : #validation }
TimeError class >> validateSeconds: seconds andNanos: nano [
	"Signal an error if seconds are outside 24 hours, or nano is larger than a second"
	(seconds between: 0 and: "24*60*60-1" 86399)
		ifFalse: [ self signal: ('Time should be in a 24 hour period. Got {1} seconds, should be between 0 and 86399' format: { seconds }) ].
	(nano between: 0 and: 1000000000-1)
		ifFalse: [ self signal:('Nano second part of time should be possitive and less than one second. Got {1} nanoseconds' format: { nano }) ]
]
