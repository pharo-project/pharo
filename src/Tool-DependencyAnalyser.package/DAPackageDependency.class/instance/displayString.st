printing
displayString
	^ String streamContents: [ :stream | self printReasonOn: stream ]