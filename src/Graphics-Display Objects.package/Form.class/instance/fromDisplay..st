initialization
fromDisplay: aRectangle 
	"Create a virtual bit map from a user specified rectangular area on the 
	display screen. Reallocates bitmap only if aRectangle ~= the receiver's 
	extent."

	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].
	self
		copyBits: (aRectangle origin extent: self extent)
		from: Display
		at: 0 @ 0
		clippingBox: self boundingBox
		rule: Form over
		fillColor: nil