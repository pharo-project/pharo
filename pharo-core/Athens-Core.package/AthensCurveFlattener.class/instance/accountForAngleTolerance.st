subdivision callbacks
accountForAngleTolerance
	" It's important only when we want to draw an equidistant curve, that is, a stroke of considerable width. If we don't need to draw a stroke or the stroke width is one pixel or less, the distance criterion works quite well. "
	^ false.