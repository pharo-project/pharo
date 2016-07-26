conversion
to: anEnd
	"Answer an Timespan. anEnd must be aDateAndTime or a Timespan" 
	| end |
	end := (anEnd isKindOf: Timespan)
		ifTrue: [ anEnd end ]
		ifFalse: [ anEnd asDateAndTime ].
	^ Timespan starting: (self start) ending: end. 