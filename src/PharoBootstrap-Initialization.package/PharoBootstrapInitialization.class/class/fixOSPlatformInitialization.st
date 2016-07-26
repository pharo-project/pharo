temporary-patch
fixOSPlatformInitialization
	"OSPlatform class>>#initialize has a nil reference compiled (DisplayScreen).
	Avoid pbs with this nil and register properly by setting ShutDownList directly"
	
	OSPlatform class compile: 'initialize
	"Initialize the receiver"
	"OSPlatform initialize"

	Smalltalk 
		addToStartUpList: self after: Delay.
	self startUp: true.'.
