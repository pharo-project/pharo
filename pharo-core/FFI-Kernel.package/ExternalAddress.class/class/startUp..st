class initialization
startUp: resuming
	"If starting the image afresh all external addresses should be zero.
	 In addition, if the word size has changed then external addresses shoiuld be resized.
	 The two steps are combined for efficiency."
	resuming ifTrue:
		[| instances |
		 instances := self allInstances.
		 wordSize ~= Smalltalk wordSize
			ifTrue: "Implement nulling by becomming all existing instances to new (and hence null) pointers of the right size."
				[wordSize := Smalltalk wordSize.
				 instances elementsForwardIdentityTo: (instances collect: [:ea| self basicNew: wordSize])]
			ifFalse:
				[instances do: [:addr| addr beNull]]]