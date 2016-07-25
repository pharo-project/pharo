drawing
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(aCanvas isVisible: self fullBounds) ifFalse:[^self].
	
	(self hasProperty: #errorOnDraw) ifTrue:[^self drawErrorOn: aCanvas].
	
	[
		"Note: At some point we should generalize this into some sort of 
		multi-canvas so that we can cross-optimize some drawing operations."
		"Pass 1: Draw eventual drop-shadow"
		self hasDropShadow ifTrue: [ self drawDropShadowOn: aCanvas ].

		"Pass 2: Draw receiver itself"
		
		aCanvas roundCornersOf: self during:[
			(aCanvas isVisible: self bounds) ifTrue:[aCanvas drawMorph: self].
			self drawSubmorphsOn: aCanvas.
			self drawDropHighlightOn: aCanvas.
			self drawMouseDownHighlightOn: aCanvas]
		
	] on: Error do: [:err |
		self setProperty: #errorOnDraw toValue: true.
		self setProperty: #drawError toValue: err freeze.
		^ self drawErrorOn: aCanvas
	]