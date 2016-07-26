analyzing
rectangleEnclosingPixelsNotOfColor: aColor
	"Answer the smallest rectangle enclosing all the pixels of me that are different from the given color. Useful for extracting a foreground graphic from its background."

	| cm slice copyBlt countBlt top bottom newH left right |
	"map the specified color to 1 and all others to 0"
	cm := Bitmap new: (1 bitShift: (self depth min: 15)).
	cm primFill: 1.
	cm at: (aColor indexInMap: cm) put: 0.

	"build a 1-pixel high horizontal slice and BitBlts for counting pixels of interest"
	slice := Form extent: width @ 1 depth: 1.
	copyBlt := (BitBlt toForm: slice)
		sourceForm: self;
		combinationRule: Form over;
		destX: 0
			destY: 0
			width: width
			height: 1;
		colorMap: cm.
	countBlt := (BitBlt toForm: slice)
		fillColor: (Bitmap with: 0);
		destRect: (0 @ 0 extent: slice extent);
		combinationRule: 32.

	"scan in from top and bottom"
	top := (0 to: height)
		detect: [ :y | 
			copyBlt
				sourceOrigin: 0 @ y;
				copyBits.
			countBlt copyBits > 0 ]
		ifNone: [ ^ 0 @ 0 extent: 0 @ 0 ].
	bottom := (height - 1 to: top by: -1)
		detect: [ :y | 
			copyBlt
				sourceOrigin: 0 @ y;
				copyBits.
			countBlt copyBits > 0 ].

	"build a 1-pixel wide vertical slice and BitBlts for counting pixels of interest"
	newH := bottom - top + 1.
	slice := Form extent: 1 @ newH depth: 1.
	copyBlt := (BitBlt toForm: slice)
		sourceForm: self;
		combinationRule: Form over;
		destX: 0
			destY: 0
			width: 1
			height: newH;
		colorMap: cm.
	countBlt := (BitBlt toForm: slice)
		fillColor: (Bitmap with: 0);
		destRect: (0 @ 0 extent: slice extent);
		combinationRule: 32.

	"scan in from left and right"
	left := (0 to: width)
		detect: [ :x | 
			copyBlt
				sourceOrigin: x @ top;
				copyBits.
			countBlt copyBits > 0 ].
	right := (width - 1 to: left by: -1)
		detect: [ :x | 
			copyBlt
				sourceOrigin: x @ top;
				copyBits.
			countBlt copyBits > 0 ].
	^ left @ top corner: (right + 1) @ (bottom + 1)