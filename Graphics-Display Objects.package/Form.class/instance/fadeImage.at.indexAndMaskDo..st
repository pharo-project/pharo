transitions
fadeImage: otherImage at: topLeft
	indexAndMaskDo: indexAndMaskBlock
	
	"This fade uses halftones as a blending hack.
	Zeros in the halftone produce the original image (self), and 
	ones in the halftone produce the 'otherImage'.
	IndexAndMaskBlock gets evaluated prior to each cycle,
	and the resulting boolean determines whether to continue cycling."
	| index imageRect maskForm resultForm |
	imageRect := otherImage boundingBox.
	resultForm := self copy: (topLeft extent: imageRect extent).
	maskForm := Form extent: 32@32.
	index := 0.

	[indexAndMaskBlock value: (index := index+1) value: maskForm]
	whileTrue:
		[maskForm reverse.
		resultForm copyBits: imageRect from: resultForm at: 0@0
			clippingBox: imageRect rule: Form over fillColor: maskForm.
		maskForm reverse.
		resultForm copyBits: imageRect from: otherImage at: 0@0
			clippingBox: imageRect rule: Form under fillColor: maskForm.
		self copyBits: imageRect from: resultForm at: topLeft
				clippingBox: self boundingBox rule: Form over fillColor: nil.
		Display forceDisplayUpdate]