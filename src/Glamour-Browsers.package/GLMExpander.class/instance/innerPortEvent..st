events
innerPortEvent: aPortEvent 
	 
	aPortEvent portName = #entity ifFalse: [
		"We resend the value to the outer pane"
		"We don't want to resend the transmission, but use a new SimpleTransmission." 
		(self pane port: aPortEvent portName) value: aPortEvent value]