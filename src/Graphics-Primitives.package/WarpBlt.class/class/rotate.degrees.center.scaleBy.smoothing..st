form rotation
rotate: srcForm degrees: angleInDegrees center: aPoint scaleBy: scalePoint smoothing: cellSize 
	"Rotate the given Form the given number of degrees about the given center and scale its width and height by x and y of the given scale point. Smooth using the given cell size, an integer between 1 and 3, where 1 means no smoothing. Return a pair where the first element is the rotated Form and the second is the position offset required to align the center of the rotated Form with that of the original. Note that the dimensions of the resulting Form generally differ from those of the original."
	| srcRect center radians dstOrigin dstCorner p dstRect inverseScale quad dstForm newCenter warpSrc |
	srcRect := srcForm boundingBox.
	center := srcRect center.
	radians := angleInDegrees degreesToRadians.
	dstOrigin := dstCorner := center.
	srcRect corners do: 
		[ :corner | 
		"find the limits of a rectangle that just encloses the rotated
		 original; in general, this rectangle will be larger than the
		 original (e.g., consider a square rotated by 45 degrees)"
		p := (corner - center scaleBy: scalePoint) + center.
		p := (p 
			rotateBy: radians
			about: center) rounded.
		dstOrigin := dstOrigin min: p.
		dstCorner := dstCorner max: p ].

	"rotate the enclosing rectangle back to get the source quadrilateral"
	dstRect := dstOrigin corner: dstCorner.
	inverseScale := (1.0 / scalePoint x) @ (1.0 / scalePoint y).
	quad := dstRect innerCorners collect: 
		[ :corner | 
		p := corner 
			rotateBy: radians negated
			about: center.
		(p - center scaleBy: inverseScale) + center ].

	"make a Form to hold the result and do the rotation"
	warpSrc := srcForm.
	srcForm isColorForm 
		ifTrue: 
			[ cellSize > 1 | true 
				ifTrue: 
					[ "ar 12/27/2001: Always enable - else sketches won't work"
					warpSrc := Form 
						extent: srcForm extent
						depth: 16.
					srcForm displayOn: warpSrc.
					dstForm := Form 
						extent: dstRect extent
						depth: 16	"use 16-bit depth to allow smoothing" ]
				ifFalse: 
					[ dstForm := srcForm class 
						extent: dstRect extent
						depth: srcForm depth ] ]
		ifFalse: 
			[ dstForm := srcForm class 
				extent: dstRect extent
				depth: srcForm depth ].
	(WarpBlt toForm: dstForm)
		sourceForm: warpSrc;
		colorMap: (warpSrc colormapIfNeededFor: dstForm);
		cellSize: cellSize;
		combinationRule: Form paint;
		copyQuad: quad
			toRect: dstForm boundingBox.	"installs a new colormap if cellSize > 1"
	dstForm isColorForm ifTrue: [ dstForm colors: srcForm colors copy ].
	newCenter := (center 
		rotateBy: radians
		about: aPoint) truncated.
	^ Array 
		with: dstForm
		with: dstRect origin + (newCenter - center)