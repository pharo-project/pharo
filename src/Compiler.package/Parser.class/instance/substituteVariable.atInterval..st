error correction
substituteVariable: each atInterval: anInterval 
	self 
		substituteWord: each
		wordInterval: anInterval
		offset: 0.
	^encoder encodeVariable: each