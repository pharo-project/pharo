*UnifiedFFI
pointer
	"Answers a pointer to the this address. 
	 Sometimes, an ExternalAddress represents a pointer, so you do not need this, but some others 
	 it represents just an address. Yes, I know, difference is subtle. 
	 Take this as an example:
		
		SQSurfaceDispatch externalNew. 

	 This will answer an instance if FFIExternalStructure with an ExternalAddress as handle, but 
	 this handle WILL NOT represent a pointer address, just an address. 
	 This is why, later in a call where I use the surface: 
	
		AthensCairoSurface>>#initializeForNewSession
	
	... I need to keep a pointer to the surface, just the external address is not enough."
	^ (ExternalAddress allocate: self size)
		pointerAt: 1 put: self;
		yourself