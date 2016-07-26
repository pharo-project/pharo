transforming
rotateBy: direction centerAt: aPoint
	"Return a copy rotated #right, #left, or #pi about aPoint"
	^ (origin rotateBy: direction centerAt: aPoint)
		rectangle: (corner rotateBy: direction centerAt: aPoint)