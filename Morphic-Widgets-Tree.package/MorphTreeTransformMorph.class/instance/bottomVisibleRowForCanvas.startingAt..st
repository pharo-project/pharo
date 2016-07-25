drawing
bottomVisibleRowForCanvas: aCanvas startingAt: aPos
     "return the bottom visible row in aCanvas's clip rectangle"
	
	^ self rowAtLocation: (aCanvas clipRect  bottomRight)  startingAt: aPos
