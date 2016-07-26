accessing
bitBlt: aBitBlt
	bitBlt := aBitBlt.
	bitBlt ifNil: [^self].
	self class primitiveSetBitBltPlugin: bitBlt getPluginName.
	self clipRect: bitBlt clipRect.
	bitBlt 
		sourceForm: (Form extent: span size @ 1 depth: 32 bits: span);
		sourceRect: (0@0 extent: 1@span size);
		colorMap: (Color colorMapIfNeededFrom: 32 to: bitBlt destForm depth);
		combinationRule: (bitBlt destForm depth >= 8 ifTrue:[34] ifFalse:[Form paint]).