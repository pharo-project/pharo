writing
writeType6DataOn: zStream
	"Write RGBA data."
	| scanline hack hackBlt cm miscBlt |
	scanline := ByteArray new: bytesPerScanline.
	hack := Form extent: width@1 depth: 32 bits: scanline.
	form depth = 16 ifTrue:[
		"Expand 16 -> 32"
		miscBlt := BitBlt toForm: hack.
		miscBlt sourceForm: form.
		miscBlt combinationRule: Form over.
		miscBlt destOrigin: 0@0.
		miscBlt width: width; height: 1.
	].
	hackBlt := BitBlt toForm: hack.
	hackBlt sourceForm: (miscBlt ifNil:[form] ifNotNil:[hack]).
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@0.
	hackBlt width: width; height: 1.
	bigEndian ifTrue:[
		cm := ColorMap 
			shifts: #(8 8 8 -24) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	] ifFalse:[
		cm := ColorMap 
			shifts: #(-16 0 16 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	].
	hackBlt colorMap: cm.
	0 to: height-1 do:[:i|
		miscBlt ifNil:[
			hackBlt sourceOrigin: 0@i; copyBits.
		] ifNotNil:[
			miscBlt sourceOrigin: 0@i; copyBits.
			hack fixAlpha.
			hackBlt copyBits.
		].
		zStream nextPut: 0. "filterType"
		zStream nextPutAll: scanline.
	].
	zStream close.