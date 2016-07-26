drawing
fullDrawOn: aCanvas

	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(aCanvas isVisible: self fullBounds) ifFalse:[^self].
	(self hasProperty: #errorOnDraw) ifTrue:[^self drawErrorOn: aCanvas].
	
	self checkSurface.

	[
		self render.
		surface displayOnMorphicCanvas: aCanvas at: bounds origin.
	
	] on: Error do: [:err |
		self setProperty: #errorOnDraw toValue: true.
		self setProperty: #drawError toValue: err freeze.
		^ self drawErrorOn: aCanvas
	]