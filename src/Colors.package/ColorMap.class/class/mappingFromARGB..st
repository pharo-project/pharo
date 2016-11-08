instance creation
mappingFromARGB: dstBitMasks
	"Return a ColorMap mapping from canonical ARGB space into dstBitMasks"
	^self mappingFrom: #(16rFF0000 16rFF00 16rFF 16rFF000000) to: dstBitMasks