color mapping
colormapToARGB
	"Return a ColorMap mapping from the receiver into canonical ARGB space."
	self hasNonStandardPalette 
		ifTrue:[^self colormapFromARGB inverseMap].
	self depth <= 8 ifTrue:[
		^ColorMap
			shifts: #(0 0 0 0)
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000)
			colors: (Color colorMapIfNeededFrom: self depth to: 32)].
	self depth = 16 ifTrue:[
		^ColorMap 
			shifts: #( 9 6 3 0) 
			masks: #(16r7C00 16r3E0 16r1F 0)].
	self depth = 32 ifTrue:[
		^ColorMap
			shifts: #(0 0 0 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000)].
	self error:'Bad depth'