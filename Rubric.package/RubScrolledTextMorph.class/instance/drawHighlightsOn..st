model protocol
drawHighlightsOn: aCanvas
	"Draw the highlights."

	| scrollBounds scrollOffset |
	
	scrollBounds := self scrollPane innerBounds.
	scrollOffset := self scrollPane scroller offset.
	
	aCanvas clipBy: self clippingBounds during: [ :canvas | 
		self highlights do: [ :highlight | 
			highlight drawOn: canvas in: scrollBounds offset: scrollOffset 
		] 
	]