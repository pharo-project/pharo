private primitives
primQsort: array with: count with: size with: compare
	self
		ffiCall: #(void qsort (FFIExternalArray array, size_t count, size_t size, FFICallback compare)) 
		module: LibC