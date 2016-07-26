drawing
drawClippedBorderOn: aCanvas usingEnds: anArray 
	aCanvas clipBy: self bounds during:[:cc| self drawBorderOn: cc usingEnds: anArray].