direction
origin: aPoint length: aLength breadth: aBreadth
	"Creates a rectangle to the right of the vector pointing in direction I define"
	^ aPoint - (aBreadth @ 0) extent: aBreadth @ aLength