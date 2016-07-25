testing
isActive
	"Answer active if fullscreen too."
	
	^self isFullscreen or: [super isActive]