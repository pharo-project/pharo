Extension { #name : 'ImageFillStyle' }

{ #category : '*Polymorph-Widgets' }
ImageFillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."

	self extent
		ifNil: [aCanvas
				translucentFormSet: self formSet
				at: self origin]
		ifNotNil: [aCanvas clipBy: (self origin + self offset extent: self extent) during: [:c |
					c
						translucentFormSet: self formSet
						at: self origin]]
]
