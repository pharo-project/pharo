drawing
draw: anObject
	"A generic dispatch method to draw anObject on receiver. 
	the object should understand #drawOnAthensCanvas: message"
	^ anObject drawOnAthensCanvas: self