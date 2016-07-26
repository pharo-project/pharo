system simulation
failPrimitiveWith: maybePrimFailToken
	"The receiver is a freshly-created context on a primitive method.  Skip the callPrimitive:
	 bytecode and store the primitive fail code if there is one and the method consumes it."
	self skipCallPrimitive.
	((self isPrimFailToken: maybePrimFailToken)
	  and: [method encoderClass isStoreAt: pc in: method]) ifTrue:
		[self at: stackp put: maybePrimFailToken last]