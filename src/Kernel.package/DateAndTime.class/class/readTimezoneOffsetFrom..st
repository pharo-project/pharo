input
readTimezoneOffsetFrom: stream
	"Read and return an optional timezone offset in the form of 
	[+|-]hh[[separator]mm[[separator]ss]] or Z from stream as a duration.
	If there is no offset, return the local offset."

	| sign hour minute second |
	(stream peekFor: $Z) ifTrue: [ ^ Duration zero ].
	hour := minute := second := 0.
	^ ('+-' includes: stream peek)
		ifTrue: [
			sign := stream next = $- ifTrue: [ -1 ] ifFalse: [ 1 ].
			hour := self readTwoDigitIntegerFrom: stream.
			(self readOptionalSeparatorFrom: stream)
				ifNotNil: [ 
					minute := self readTwoDigitIntegerFrom: stream.
					(self readOptionalSeparatorFrom: stream)
						ifNotNil: [ 
							second := Integer readFrom: stream ] ].
			Duration seconds: sign * ((hour * 3600) + (minute * 60) + second) ]
		ifFalse: [ self localOffset ]