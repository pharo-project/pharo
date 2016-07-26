operations
insetRectangle: aRectangle
	"Answer a rectangle whose size has been reduced by the receiver. The limitation is that since a rectangle topleft is always less than its bottomright you may have a some cases that are not possible to express. For that you should use a margin object instead of a rectangle."
	
	| l r t b|
	l := aRectangle left +  self left.
	r :=aRectangle right -  self right.
	t := aRectangle top + self top.
	b := aRectangle bottom - self bottom.
	^ Rectangle origin:  l @t extent: (r - l @ (b - t )) 
