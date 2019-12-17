Extension { #name : #DateAndTime }

{ #category : #'*Deprecated80' }
DateAndTime class >> fromSqueakTime: anInteger [
	"Answer an instance of the receiver with the time represented by anInteger.
	Squeak time is in the local timezone with an epoch of 1 Jan. 1901"
	
	self deprecated: 'Use #fromInternalTime: instead.' transformWith: '`@receiver fromSqueakTime: `@arg' -> '`@receiver fromInternalTime: `@arg'.
	^ self fromInternalTime: anInteger
]
