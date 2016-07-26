drawing
athensFillRectangle: aRect on: aCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	|o e toFillRect|
	o := origin ifNil:[aRect origin] ifNotNil:[origin].
	e := extent ifNil:[aRect extent] ifNotNil:[extent].
	toFillRect := o extent:e.
	
	aCanvas
		setSourceR: r g: g b: b a: a; 
		newPath;
		rectangleX: toFillRect left y: toFillRect top width: toFillRect width height: toFillRect height;
		fill.