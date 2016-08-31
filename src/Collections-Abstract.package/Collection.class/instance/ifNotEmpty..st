testing
ifNotEmpty: aBlock
	"Evaluate the given block with the receiver as argument, answering its value
    unless the receiver is empty, in which case answer the receiver."

    ^self isEmpty
          ifTrue: [self]
          ifFalse: [aBlock cull: self] 
