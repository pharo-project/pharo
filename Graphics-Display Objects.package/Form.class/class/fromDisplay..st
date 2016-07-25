instance creation
fromDisplay: aRectangle 
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen defined by aRectangle."

	^ (self extent: aRectangle extent depth: Display depth)
		fromDisplay: aRectangle