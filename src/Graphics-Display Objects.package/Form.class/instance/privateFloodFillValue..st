private
privateFloodFillValue: aColor
	"Private. Compute the pixel value in the receiver's depth but take into account implicit color conversions by BitBlt."
	| f1 f2 bb |
	f1 := Form extent: 1@1 depth: depth.
	f2 := Form extent: 1@1 depth: 32.
	bb := BitBlt toForm: f1.
	bb fillColor: aColor; 
		destRect: (0@0 corner: 1@1); 
		combinationRule: 3; 
		copyBits.
	bb := BitBlt toForm: f2.
	bb sourceForm: f1; 
		sourceOrigin: 0@0;
		destRect: (0@0 corner: 1@1);
		combinationRule: 3;
		copyBits.
	^f2 pixelValueAt: 0@0.