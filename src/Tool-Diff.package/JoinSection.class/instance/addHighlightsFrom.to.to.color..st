adding
addHighlightsFrom: srcBlock to: dstBlock to: aCollection color: aColor
	"Add the highlights required for the given character blocks
	of a paragraph. May be up to three highlights depending
	on the line spans."
	
	srcBlock textLine = dstBlock textLine
		ifTrue: [aCollection add: (TextHighlightByBounds new
				color: aColor;
				bounds: (srcBlock topLeft corner: dstBlock bottomRight))]
		ifFalse: [aCollection
					add: (TextHighlightByBounds new
							color: aColor;
							bounds: (srcBlock topLeft corner: srcBlock textLine bottomRight));
					add: (TextHighlightByBounds new
							fillWidth: true;
							color: aColor;
							bounds: (srcBlock bottomLeft corner: dstBlock topRight));
					add: (TextHighlightByBounds new
							color: aColor;
							bounds: (dstBlock textLine topLeft corner: dstBlock bottomRight))]