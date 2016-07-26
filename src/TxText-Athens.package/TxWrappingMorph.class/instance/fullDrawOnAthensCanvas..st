drawing
fullDrawOnAthensCanvas: aCanvas
	"nothing"
	
	aCanvas pathTransform restoreAfter: [ 
		aCanvas pathTransform translateBy: owner innerBounds origin.
		super fullDrawOnAthensCanvas: aCanvas.
	].

