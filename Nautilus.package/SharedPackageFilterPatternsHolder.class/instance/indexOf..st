accessing
indexOf: aString 
	
	^ mutex critical: [ value indexOf: aString ]