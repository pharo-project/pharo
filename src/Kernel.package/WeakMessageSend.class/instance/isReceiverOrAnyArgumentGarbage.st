private
isReceiverOrAnyArgumentGarbage
	"Make sure that my receiver hasn't gone away"
	^self isReceiverGarbage 
		or: [self isAnyArgumentGarbage]
