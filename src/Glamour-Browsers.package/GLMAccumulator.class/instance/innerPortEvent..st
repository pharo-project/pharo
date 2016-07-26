events
innerPortEvent: aPortEvent 
	aPortEvent portName = #entity ifFalse: [
		"We don't want to resend the transmission, but use a new SimpleTransmission." 
		(self pane port: aPortEvent portName) value: aPortEvent value]