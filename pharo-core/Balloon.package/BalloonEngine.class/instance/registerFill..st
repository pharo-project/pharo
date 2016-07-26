drawing
registerFill: aFillStyle
	"Register the given fill style."
	| theForm |
	aFillStyle ifNil:[^0].
	aFillStyle isSolidFill 
		ifTrue:[^aFillStyle scaledPixelValue32].

	aFillStyle isGradientFill ifTrue:[
		^self primAddGradientFill: aFillStyle pixelRamp
			from: aFillStyle origin
			along: aFillStyle direction
			normal: aFillStyle normal
			radial: aFillStyle isRadialFill
		].
	aFillStyle isBitmapFill ifTrue:[
		theForm := aFillStyle form asSourceForm.
		theForm unhibernate.
		forms := forms copyWith: theForm.
		^self primAddBitmapFill: theForm
				colormap: (theForm colormapIfNeededForDepth: 32)
				tile: aFillStyle isTiled
				from: aFillStyle origin
				along: aFillStyle direction
				normal: aFillStyle normal
				xIndex: forms size].
	^0