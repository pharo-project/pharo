analyzing
primCountBits
	"Count the non-zero pixels of this form."
	self depth > 8 ifTrue:
		[^(self asFormOfDepth: 8) primCountBits].
	^ (BitBlt toForm: self)
		fillColor: (Bitmap with: 0);
		destRect: (0@0 extent: width@height);
		combinationRule: 32;
		copyBits