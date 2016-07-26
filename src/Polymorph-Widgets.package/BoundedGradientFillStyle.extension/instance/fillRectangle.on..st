*Polymorph-Widgets
fillRectangle: aRectangle on: aCanvas
	"Fill the given rectangle on the given canvas with the receiver."
	
	self extent ifNil: [^super fillRectangle: aRectangle on: aCanvas].
	aCanvas fillRectangle: ((self origin extent: self extent) intersect: aRectangle ifNone: [^ self ]) basicFillStyle: self