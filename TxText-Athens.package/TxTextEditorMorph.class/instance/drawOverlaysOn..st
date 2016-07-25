drawing
drawOverlaysOn: aCanvas
	"draw an overlays (after all submorphs being drawn)"
	
	| pathTransform |
	pathTransform := aCanvas pathTransform.
	pathTransform restoreAfter: [
		pathTransform translateBy: self innerBounds topLeft.  
		
		self hasFocus ifTrue: [ 
			aCanvas setPaint: (Color yellow alpha:0.05).
			aCanvas drawShape: (0@0 extent: self innerBounds extent)
			 ]
		
	"	self drawCursorPositionOn: aCanvas."
	].
