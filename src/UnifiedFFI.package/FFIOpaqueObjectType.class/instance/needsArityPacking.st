testing
needsArityPacking
	">1 because first is this object, who is a reference (even if not a pointer, is 
	 represented like that because he lives in the C heap. So, we 'roll' if it is 
	 a 'type**' or bigger"
	^ self pointerArity > 1