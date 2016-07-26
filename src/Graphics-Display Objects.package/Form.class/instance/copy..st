copying
copy: aRect
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	| newForm |
	newForm := self class extent: aRect extent depth: depth.
	^ newForm copyBits: aRect from: self at: 0@0
		clippingBox: newForm boundingBox rule: Form over fillColor: nil