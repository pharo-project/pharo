displaying
displayResourceFormOn: aForm
	"a special display method for blowing up resource thumbnails"
	
	self extent = aForm extent
		ifTrue: [ ^ self displayOn: aForm ].

	"We've got no bilinear interpolation. Use WarpBlt instead"
	(WarpBlt current toForm: aForm)
		sourceForm: self destRect: aForm boundingBox;
		combinationRule: 3;
		cellSize: 2;
		warpBits.
