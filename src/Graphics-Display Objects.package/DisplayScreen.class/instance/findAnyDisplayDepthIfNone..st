private
findAnyDisplayDepthIfNone: aBlock
	"Return any display depth that is supported on this system.
	If there is none, evaluate aBlock.
	First check higher values to avoid depth 1 to be always returned."
	^ #(32 16 8 4 2 1 -32 -16 -8 -4 -2 -1) 
		detect:[ :bpp| self supportsDisplayDepth: bpp ]
		ifNone: [ aBlock value ]