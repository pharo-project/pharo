initialization
setUp
	super setUp.
	prototypes add: TextAlignment centered;
		 add: TextAlignment justified;
		 add: TextAlignment leftFlush;
		 add: TextAlignment rightFlush 