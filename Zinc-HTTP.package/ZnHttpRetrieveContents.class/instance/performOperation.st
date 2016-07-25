operations
performOperation
	"Download and return the HTTP(S) resource that url refers to.
	This will typically return a String or a ByteArray (see ZnClient>>#get:).
	If something goes wrong, an exception will be signalled."

	^ ZnClient new
		beOneShot;
		enforceHttpSuccess: true;
		get: self url