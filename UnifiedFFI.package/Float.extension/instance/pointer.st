*UnifiedFFI
pointer 
	"Floats in Pharo are 64bits (8 bytes) and that correspond directly with a double on C.
	 So 'default' behaviour will be to address it as a double (float is also available in case is 
	 needed)"
	^ self pointerDouble