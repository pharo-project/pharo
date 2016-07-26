displaying
copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf 
	(BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: (clipRect intersect: clippingBox ifNone: [ ^ self ])) copyBits