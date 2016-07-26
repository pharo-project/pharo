accessing
dateAndTime
	| aTimestamp |
	self infoAt: #dateAndTime ifPresent: [ :obj | ^ obj ] ifAbsent: [ "ignore" ].
	aTimestamp := self timestamp.
	aTimestamp ifNil: [ ^ aTimestamp ].
	
	aTimestamp isNumber 
		ifTrue: [ "version 1 keeps unix time"
			"We do not have information about time zone."
			^ self 
				infoAt: #dateAndTime 
				put: ((DateAndTime fromUnixTime: aTimestamp) translateTo: 0) ].
	
	"since version 2, timestamp keeps DateAndTime object."
	^ aTimestamp