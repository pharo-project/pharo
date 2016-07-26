events
innerPortEvent: aPortEvent
	| wantedTransmissions |
	aPortEvent hasChanged
		ifFalse: [ ^ self ].
	wantedTransmissions := self transmissions
		select: [ :each | (each originatesAt: aPortEvent port) and: [ (aPortEvent transmissionContext includesPort: each destination) not ] ].
	wantedTransmissions do: [ :each | each transmitIn: aPortEvent transmissionContext copy ]