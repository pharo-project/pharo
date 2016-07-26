drawing
drawClippedBorderOnAthensCanvas: aCanvas usingEnds: anArray
	aCanvas clipBy: self bounds during: [ self drawBorderOnAthensCanvas: aCanvas usingEnds: anArray ]