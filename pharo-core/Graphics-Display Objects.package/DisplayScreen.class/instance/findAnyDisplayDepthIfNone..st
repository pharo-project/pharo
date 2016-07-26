private
findAnyDisplayDepthIfNone: aBlock
	"Return any display depth that is supported on this system.
	If there is none, evaluate aBlock."
	#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) do:[:bpp|
		(self supportsDisplayDepth: bpp) ifTrue:[^bpp].
	].
	^aBlock value