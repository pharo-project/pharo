transforming
flipBy: direction centerAt: aPoint 
	"Return a copy flipped #vertical or #horizontal, about aPoint."
	^ (origin flipBy: direction centerAt: aPoint)
		rectangle: (corner flipBy: direction centerAt: aPoint)