instance creation
fromFreetypeFace: aFace
	| handle cairoFace |
	
	handle := aFace handle nbUlongAt: 1.
	cairoFace := self primFtFace: handle loadFlags: ( LoadNoHinting | LoadTargetLCD | LoadNoAutohint | LoadNoBitmap). 
	
	^ cairoFace initializeWithFreetypeFace: aFace