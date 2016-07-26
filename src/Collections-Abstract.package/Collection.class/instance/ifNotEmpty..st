testing
ifNotEmpty: aBlock
	"Evaluate the given block with the receiver as argument, answering its value
    unless the receiver is empty, in which case answer the receiver.

	Note that the fact that this method returns its receiver in case the receiver is empty
	allows one to write expressions like the following ones:
		self classifyMethodAs: (myProtocol ifEmpty: ['As yet unclassified']"
	
    ^self isEmpty
          ifTrue: [self]
          ifFalse: [aBlock cull: self] 
