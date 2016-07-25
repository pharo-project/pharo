events
outerPortEvent: aPortEvent 
	"We only treat the #entity port, and for it we simply trigger 
	a transmission to the default pane"

	super outerPortEvent: aPortEvent.
	aPortEvent port name = #entity ifFalse: [^self].
	self newTransmission
		addActiveOrigin: (aPortEvent port); 
		destination: (self panes anyOne port: #entity); 
		transmit