drawing
fullDrawOnAthensCanvas: aCanvas
	| clip |
	super fullDrawOnAthensCanvas: aCanvas.
	clip := self clippingBounds.

	aCanvas 
		clipBy: clip
		during: [ self drawOverlaysOn: aCanvas ]