instance creation
fromForm: aForm
	| form surface newBits |
	
	form := aForm unhibernate; asFormOfDepth: 32.
	surface := self extent: aForm extent.
	
	"we should convert form bits with premultiplied alpha"
	
	newBits := form bits collect:[:pixel |
		| alpha r g b|
		alpha := (pixel >> 24) / 255.
		
		r := ( (pixel bitAnd: 255) * alpha ) asInteger.
		g := ( (pixel >>8 bitAnd: 255) * alpha ) asInteger.
		b := ( (pixel >>16 bitAnd: 255) * alpha ) asInteger.
		
		(pixel bitAnd: 16rFF000000) + (b<<16) + (g<<8) + r
		].
	
	LibC memCopy: newBits to: surface getDataPtr getHandle size: (form width * form height *4).
	
	surface markDirty.

	^ surface.