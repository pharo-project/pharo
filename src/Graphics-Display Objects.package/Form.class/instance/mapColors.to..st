color mapping
mapColors: oldColorBitsCollection to: newColorBits
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	self depth < 16
		ifTrue: [map := (Color cachedColormapFrom: self depth to: self depth) copy]
		ifFalse: [
			"use maximum resolution color map"
			"source is 16-bit or 32-bit RGB; use colormap with 5 bits per color component"
			map := Color computeRGBColormapFor: self depth bitsPerColor: 5].
	oldColorBitsCollection do:[ :oldColor | map at: oldColor put: newColorBits].

	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits.
