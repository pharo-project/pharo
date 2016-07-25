other
translateBy: aPoint clippingTo: aRect during: aBlock
	self apply: [ :clippedCanvas |
		clippedCanvas translateBy: aPoint clippingTo: aRect during: aBlock ]