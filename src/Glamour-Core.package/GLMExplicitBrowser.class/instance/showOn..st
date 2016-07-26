scripting
showOn: aDestinationPane 
	"send this message when at the beginning of specifying a Transmission" 
	 
	self addTransmission: (
		GLMTransmission new
			ensureReplacePresentationsStrategy;
			destination: (self resolveDestinationPort: aDestinationPane); 
			yourself)