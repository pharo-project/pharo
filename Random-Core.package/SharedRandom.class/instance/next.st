accessing
next
	^ 	mutex critical: [ super privateNextValue ].