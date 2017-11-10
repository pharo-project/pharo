Extension { #name : #ImageFillStyle }

{ #category : #'*Polymorph-Widgets' }
ImageFillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."
	
	self extent
		ifNil: [aCanvas
				translucentImage: self form
				at: self origin]
		ifNotNil: [aCanvas clipBy: (self origin + self offset extent: self extent) during: [:c |
					c
						translucentImage: self form
						at: self origin]]
]
