drawing
drawSubmorphsOn: aCanvas
	submorphs ifEmpty: [^ self].
	aCanvas transformBy: transform
		clippingTo: (aCanvas clipRect intersect: (owner clippingBounds) ifNone: ["we're done here" ^ self ])
		during: [:myCanvas |  | top bottom |
			top := self topVisibleRowForCanvas: myCanvas.
			bottom := self bottomVisibleRowForCanvas: myCanvas startingAt: top.
			bottom 
				to: top
				by: -1			
				do: [:row | | m |
					m := submorphs basicAt: row.
					self drawRawColorOn: myCanvas forSubmorph: m.
					myCanvas fullDrawMorph: m] ]
		smoothing: smoothing.
	owner withTreeLines ifTrue: [owner drawLinesOn: aCanvas].
	owner enabled ifFalse: [ aCanvas fillRectangle: owner innerBounds fillStyle: (owner paneColor alpha: 0.2) ]
