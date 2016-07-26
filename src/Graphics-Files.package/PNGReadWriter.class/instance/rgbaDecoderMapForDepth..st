pixel copies
rgbaDecoderMapForDepth: decoderDepth
	bigEndian ifTrue:[
		depth = 16 ifTrue:[
			"Big endian, 32 -> 16 color mapping."
			^ColorMap
				shifts: #(-17 -14 -11 0)
				masks: #(16rF8000000 16rF80000 16rF800 16r00)
		] ifFalse:[
			"Big endian, 32 -> 32 color mapping"
			^ColorMap 
				shifts: #(-8 -8 -8 24) 
				masks: #(16rFF000000 16rFF0000 16rFF00 16rFF).
		].
	].
	depth = 16 ifTrue:[
		"Little endian, 32 -> 16 color mapping."
		^ColorMap
			shifts: #(7 -6 -19 0)
			masks: #(16rF8 16rF800 16rF80000 0)
	] ifFalse:[
		"Little endian, 32 -> 32 color mapping"
		^ColorMap 
			shifts: #(-16 0 16 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	].