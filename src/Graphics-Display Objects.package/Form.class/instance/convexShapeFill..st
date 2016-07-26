filling
convexShapeFill: aMask 
	"Fill the interior of the outtermost outlined region in the receiver.  The outlined region must not be concave by more than 90 degrees.  Typically aMask is Color black, to produce a solid fill. then the resulting form is used with fillShape: to paint a solid color.  See also anyShapeFill"
	| destForm tempForm |
	destForm := Form extent: self extent.  destForm fillBlack.
	tempForm := Form extent: self extent.
	(0@0) fourNeighbors do:
		[:dir |  "Smear self in all 4 directions, and AND the result"
		self displayOn: tempForm at: (0@0) - self offset.
		tempForm smear: dir distance: (dir dotProduct: tempForm extent) abs.
		tempForm displayOn: destForm at: 0@0
			clippingBox: destForm boundingBox
			rule: Form and fillColor: nil].
	destForm displayOn: self at: 0@0
		clippingBox: self boundingBox
		rule: Form over fillColor: aMask