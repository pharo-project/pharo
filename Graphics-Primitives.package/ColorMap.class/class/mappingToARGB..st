instance creation
mappingToARGB: srcBitMasks
	"Return a ColorMap mapping from srcBitMasks into canonical ARGB space"
	^self mappingFrom: srcBitMasks to: #(16rFF0000 16rFF00 16rFF 16rFF000000)