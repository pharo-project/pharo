access
interval: d
	"Supply an optional floating fraction so slider can expand to indicate range.
	Update the fill style for the tumb (may have extent-based elements)."
	
	|oldExtent|
	oldExtent := slider extent.
	interval := d min: 1.0.
	self expandSlider.
	self computeSlider.
	slider extent = oldExtent
		ifFalse: [slider fillStyle: self normalThumbFillStyle]