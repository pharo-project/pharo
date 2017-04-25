instance creation
fromHandle: aHandle
	"Offsets are calculated when specs are compiled, but sometimes there is a possibility that 
	 user access them before actually using the compiled spec, so I need to be sure they are 
	 initialised before anything. This is why I send #compiledSpec here."
	self compiledSpec. 
	^ (super fromHandle: aHandle) initialize 