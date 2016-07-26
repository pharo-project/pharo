image manipulation
smear: dir distance: dist
	"Smear any black pixels in this form in the direction dir in Log N steps"
	| skew bb |
	bb := BitBlt destForm: self sourceForm: self fillColor: nil
		combinationRule: Form under destOrigin: 0@0 sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox.
	skew := 1.
	[skew < dist] whileTrue:
		[bb destOrigin: dir*skew; copyBits.
		skew := skew+skew]