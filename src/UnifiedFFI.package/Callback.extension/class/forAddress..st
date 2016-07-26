*UnifiedFFI
forAddress: address 
	"Reconstructs a callback from an address (an integer). 
	 This is useful for reading callbacks from a structure. 
	 WARNING: Of course, the callback needs to exist before :)"
	| callback |
	
	callback := ThunkToCallbackMap 
		at: address 
		ifAbsent: [ self error: 'No callback for address 16r', address printStringHex, '.' ].	
	callback ifNil: [ 
		self error: 'Callback at address 16r', address printStringHex, ' has been collected.' ].
		
	^ callback