testing
ifEmpty: aBlock
	"Evaluate the given block with the receiver as argument, answering its value if the receiver is empty, otherwise answer the receiver."
	
	"Note that the fact that this method returns its argument in case the receiver is not empty allows one to write expressions like the following ones: self classifyMethodAs: (myProtocol ifEmpty: ['As yet unclassified'])"

	^ self isEmpty 
		ifTrue: [ ^aBlock value ]
		ifFalse: [ self ]