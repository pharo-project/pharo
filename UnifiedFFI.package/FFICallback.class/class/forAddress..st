instance creation
forAddress: address 
	"Reconstructs a callback from an address (an integer). 
	 This is useful for reading callbacks from a structure. 
	 WARNING: 
		Of course, the callback needs to exist before :)
		And the callback needs to be created with FFICallback (not plain Alien callbacks), then 
		we can be sure block in fact contains the FFICallback instance"
	^ (Callback forAddress: address) evaluator