instance creation
forPointer: address "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: 0;
		addressFieldPut: address;
		yourself