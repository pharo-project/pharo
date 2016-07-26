testing
includes: aDateAndTime 

	^ (aDateAndTime isKindOf: Timespan)
			ifTrue: [ 
				(self includes: aDateAndTime start)
					and: [ self includes: aDateAndTime end ] ]
			ifFalse: [ aDateAndTime asDateAndTime between: start and: self end ]
