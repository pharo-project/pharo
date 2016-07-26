events
outerPortEvent: aPortEvent 
	
 	super outerPortEvent: aPortEvent.
	aPortEvent port name = #entityToSelect ifTrue: [
		self tryToSelectPaneFrom: aPortEvent port ].	
	aPortEvent port name = #entity ifTrue: [
		self addPaneFrom: aPortEvent port ]