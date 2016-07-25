testing
needsArityUnpacking
	"Simple types do not need ''unpacking'' because they can not used as buffers (to receive values 
	 from C functions).
	 For instance, in case you have functions with the form:
	 [[[ 
		self ffiCall: #(void getPoint( double *x, double *y)) 
	 ]]]
	 you cannot use instances of Float (since they are immutable in Pharo)... in that case you will
	 need to use an FFIExternalValueHolder."
	^ false