clipping
clipBy: aRectangle during: aBlock
	
	"Set a clipping rectangle during drawing operations performed in aBlock.
	Note that the clipping rectangle is intersected with the currently active clipping rectangle"
	
	 ^surface clipBy: aRectangle during: aBlock