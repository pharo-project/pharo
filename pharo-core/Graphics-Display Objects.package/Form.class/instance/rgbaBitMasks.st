color mapping
rgbaBitMasks
	"Return the masks for specifying the R,G,B, and A components in the receiver"
	self depth <= 8
		ifTrue:[^#(16rFF0000 16rFF00 16rFF 16rFF000000)].
	self depth = 16
		ifTrue:[^#(16r7C00 16r3E0 16r1F 16r0)].
	self depth = 32
		ifTrue:[^#(16rFF0000 16rFF00 16rFF 16rFF000000)].
	self error:'Bad depth for form'