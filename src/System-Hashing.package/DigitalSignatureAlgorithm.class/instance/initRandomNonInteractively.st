initialization
initRandomNonInteractively

 	self initRandomFromString: 
 		Time millisecondClockValue printString, 
 		Date today printString, 
 		Smalltalk os platformName printString